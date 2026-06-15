# Implementation

CapyScheme aims to implement R6RS/R7RS specification and also be performant. Implementation of language
consists of "core expander", CPS compiler, runtime, and GC.

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

These forms all expand into Tree IR which is based on what Guile has. Tree IR has following forms:

- `(lref <lvar>)`: loads local variable `<lvar>`
- `(lset <lvar> <term>)`: updates local variable `<lvar>` to the result of executing `<term>`.
- `(module-ref <module> <name> <public?>)`: lookups `<name>` in `<module>` or its public interface if `public?` is `#t`.
- (module-set! <module> <name> <public?> <term>):
  lookups `<name>` in `<module>` or its public interface if `public?` is `#t`
  and then sets it to result of executing `<term>`
- `(toplevel-ref <module> <name>)`: lookups `<name>` in `<module>`
- `(toplevel-set! <module> <name> <term>)`: lookups `<name>` in `<module>` and updates its binding
  to the value of `<term>`.
- `(toplevel-define <module> <name> <term>)`: defines variable `<name>` in `<module>` and binds
  its value to the value of `<term>`
- `(primref <name>)`: lookups primitive in `(capy)` modules and returns its reference
- `(primcall <name> <term*> ...)` : calls primitive in `(capy)` module with `<term*> ...` arguments. Internally primitive calls might be expanded into simpler forms or converted to `(call (module-ref ...) ...)` when some primitive conditions are violated e.g argument count is wrong.
- `(if <test> <cons> <alt>)`: if `<test>` term evaluates to `#t` will jump to `<cons>`, otherwise
  executes `<alt>`.
- `(cons <value>)`: self-evaluating form which returns `<value>`.
- `(receive <bindings> <producer> <consumer>)`: receives multiple values from `<producer>`, binds
  them to `<bindings>`, and executes `<consumer>`.
- `(fix <procs> <body>)`: binds mutually recursive procedures `<procs>`, and executes `<body>`. This form is produced only after fixing letrec pass.
- `(let <style> <lhs> <rhs> <body>)`: let-form, `style` indicates if its letrec, let\* or anything else. Executes `<rhs>` expressions and binds them to `<lhs>`, and then runs `<body>`.
- `(proc <args> <variadic> <body>)`: produces a procedure value with `args` and `variadic` being optional variadic argument binder.

### Optimizations on Tree IR

Tree IR has a few passes and optimizations that are applied, and some of them are necessary:

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

#### Assignment elimination

This pass removes `lset` forms and replaces them by boxes. Each mutable variable is wrapped by `box`, and accesses are converted to `box-ref` while assignment are converted to `box-set!`.

#### Primitive resolution

This pass must run to produce faster code. It detects calls to primitives, and converts them to `primcall` or `primtref` in case of references. Later on, primcalls can be lowered directly to machine code rather than always calling into runtime.

#### Primitive expansion

Performs expansion of `primcall` forms into simpler forms where possible, also sometimes undoes work of primitive resolution by converting primcalls back into regular calls. Some examples of optimizations:

```
(memq x '(a b c)) => (if (eq? x 'a) #t (if (eq? x 'b) #t (if (eq? x 'c) #t) #f))
(+ 1 2 3 4) => (+ (1 2) (+ 3 4))
```

If argument count to primitive does not match this pass will convert primitive back to regular call.

### CPS stage

CapyScheme relies on CPS in order to get cheap first-class continuations and easy exception handling. Tree IR is converted to CPS after its optimized and CPS form has the following terms and atoms:

```scm
; atoms
(local <lvar>)  ; loads local variable <lvar>
(const <value>) ; self evaluating form

; expressions

(primcall <prim> <handler-k> <args> ...) ; invokes primitive function and in case of error continues execution to `handler-k` continuation.

; terms

(let <var> <expression> <term>) ; evaluates expression, binds result of evaluating it to `var` and continues execution in `term`.

(letk (<kname> <args> <kbody>) <body>) ; binds continuations and then resumes execution in `<body>`. This
term allows continuations to be mutually recursive

(fix (<fname> <fargs> <fbody>) <body>) ; analagous to `fix` in Tree IR, does exact same thing

(cond <atom> => <kcons> | <kalt>) ; if <atom> is truthy continues to continuation kcons, otherwise to kalt

(continue <k> <args>) ; continues execution to continuation `k` with `args`.

(call <proc> <retk> <reth> <args>) ; calls `proc` with return and handler continuations `retk` and `reth` respectivelly.

```

#### CPS conversion

Tree IR is converted to CPS IR using higher-order transform described in [How to compile with continuations](https://matt.might.net/articles/cps-conversion/). Using higher-order transform allows us to get optimal CPS code without doing tree reductions. Some of the primitive-calls are converted directly to CPS while others remain primcalls. Example of such conversion is `car` operation which roughly expands to:

```
        letk cold (h) not_pair () = assertion_violation(cps, src, op, "not a pair", &[x], h) in
        letk (h) k () = {
            let x = #%car (x) in
            continue ret(x)
        } in
        letk (h) check_pair_class () =  {
            let pair_class = #% %class-id? (h, x, Value::new(PAIR_CLASS_ID)) in
            if pair_class => k | not_pair
        } in

        let is_immediate = #% immediate? (h, x) @ src;
        if is_immediate => not_pair | check_pair_class
```

While it does again produce primcall term in CPS form, primcall of `car` in CPS IR directly expands to memory load at specific offset.

Globals references are lowered to sequence of primitive calls like

```
let cell = #%cache-cell 'variable
let is-obj = #%heap-object? cell
letk kinit () = {
    let var = #%lookup('variable)
    let _.  = #%cache-set! 'variable var
    let value = #%variable-ref(cell)
    continue k (value)
} in
letk kok () = {
    let value = #%variable-ref(cell)
    continue k (value)
} in
cond is-obj => kok | kinit
```

This allows us to have fast and efficient global accesses once they are cached.

#### CPS optimizations

We include a few important optimizations into the compiler: tree reductions, inlining, loop unrolling and contification.

- Tree-reduction: removes beta redexes, constant folds operations and overall tries to shrink tree
  as much as possible.
- Inlining & loop unrolling: run together and inline known function calls, in case of recursive calls loop-unrolling is able to unroll calls until some limit is reached. Limit for both unrolling and inlining is computed by comparing tree size with predefined constants (fibonacci sequence at the moment).
- Contification: converts procedures into continuations which in turn allows them to be compiled as basic-block instead of requiring allocating a closure.
- Reification: marks continuations which still require a closure to be allocated. Such continuations are usually handler/return continuations passed to `call` term or used for `call/cc`.

#### CPS->SSA conversion

CPS is converted to SSA by building Cranelift IR after all optimizations are applied. During this stage
all continuations that were not reified are converted to basic-blocks, and all procedures/reified continuations are converted to Cranelift functions. Some primcalls are lowered directly to Cranelift IR, while others might be lowered as calls to runtime.

### Linking

Once SSA is built we emit object file and link using platform linker as shared object.

## Runtime

### Object headers and class ids

Heap objects carry a compact class id in the object header. The id is used by
`class-of`, generic dispatch, slot access, and GC layout lookup. Built-in classes
use reserved ids from `capy/src/rsgc/object.rs` (`builtin_class_ids`); dynamic
Scheme classes and Rust type classes are allocated after the built-in range.

Class ids are limited to 24 bits (`MAX_CLASS_ID`). The class table is paged, so
ids can be sparse without allocating one large descriptor array. A descriptor is
looked up through `class_table(ctx).lookup(id)`.

Rust heap types that implement `Trace` normally get allocation hooks through
`AllocationHooksOf<T>`. Those hooks describe:

- how to trace the object,
- how to process weak references,
- fixed or dynamic instance size and alignment,
- the Rust type name used for internal type classes.

For generic Rust types, class ids are assigned through the `generic_static`
utility. A namespace created with `define_namespace!` gives each monomorphized
`T` its own zero-initialized static cell, so `ClassTagged`/allocation code can
store per-type ids even when the type carries GC lifetimes. This currently relies
on inline assembly support for the platforms listed in
`capy/src/utils/generic_static.rs`; unsupported platforms fail at compile time.

### Class table and descriptors

`capy/src/runtime/class/` contains the object system implementation:

- `descriptor.rs` defines `ClassDescriptor`, the first-class class object.
- `table.rs` owns registration, lookup, hierarchy computation, and class
  redefinition state.
- `slot.rs` defines slot and accessor metadata.
- `generic.rs` implements generic procedures, method descriptors, dispatch
  caches, and next-method descriptors.
- `ops.rs` exposes the Rust operations to Scheme through the `(capy)` module.

Every class descriptor records its name, category, flags, direct superclasses,
class precedence list (CPL), direct slots, inherited slots, accessors, class
allocated slot storage, weak direct-subclass links, direct methods, and accepted
initargs. Scheme classes implicitly inherit from `<object>` when no explicit
root object superclass is supplied.

Class registration has three main paths:

1. `init_builtin_classes(ctx)` registers the fixed built-in classes during
   runtime initialization.
2. `make-class`/`define-class` allocate malleable Scheme classes.
3. Internal Rust types may record pending type classes before the class table is
   initialized; pending registrations are drained once built-ins are ready.

The runtime keeps old class descriptors usable during redefinition. A stale
instance can be detected with `%class-redefined?`; `touch-instance!` updates the
instance to the current class shape. Slot values with matching names are
preserved when classes are redefined or when `change-class` moves an instance to
another Scheme class.

### Scheme-facing object system

Most user-visible object-system forms are defined in `lib/boot/base.scm` on top
of the Rust primitives. A minimal class looks like:

```scheme
(import (capy))

(define-class point ()
  ((x #:init-value 0)
   (y #:init-value 0)))

(define p (make point #:x 10))

(slot-ref p 'x) ; => 10
(slot-ref p 'y) ; => 0
(slot-set! p 'y 20)
(eq? (class-of p) point) ; => #t
```

Slot options accepted by `define-class` include:

- `#:init-value`, `#:init-thunk`, `#:initform`, and `#:init-form` for defaults,
- `#:init-keyword` to rename the initarg,
- `#:immutable #t` to reject mutation after initialization,
- `#:initializable #f` to hide the slot from initargs,
- `#:allocation #:class` for storage shared by instances of the class,
- `#:slot-ref`, `#:slot-set!`, and `#:slot-bound?` for custom Scheme hooks,
- `#:getter`, `#:setter`, and `#:accessor` to generate generic accessors.

Gauche-style colon options such as `:init-value`, `:allocation`, and `:accessor`
are accepted by the macro and normalized before runtime class creation.

Slot operations dispatch through generics:

- `slot-ref` calls `slot-ref-using-class`,
- `slot-set!` calls `slot-set-using-class!`,
- `slot-bound?` calls `slot-bound-using-class?`.

Those generic hooks receive slot definition objects, which allows Scheme code to
specialize reads, writes, and bound checks while still using `next-method` to
fall back to the default storage behavior.

### Generics and methods

Generic procedures are invocable Scheme closures backed by `GenericDescriptor`.
The closure carries the descriptor in its free variables/properties so predicates
such as `generic?`, `generic-name`, and `generic-methods` work on the callable
value.

Methods record:

- the owning generic,
- a prefix list of class specializers,
- the required argument count,
- the method body,
- flags such as `:locked`/`#:locked`.

Generic dispatch takes the first required dispatch arguments, resolves their
classes, finds applicable methods by subclass/CPL checks, sorts them by
specificity, and caches the resulting method list for the dispatch class tuple.
Adding or replacing a method clears the dispatch cache. A sealed generic rejects
new or replacement methods until it is unsealed.

Example:

```scheme
(define-class point () ((x #:init-value 0)))
(define-class colored-point (point) ((color #:init-value 'black)))

(define-generic describe)

(define-method (describe (p point))
  (list 'point (slot-ref p 'x)))

(define-method (describe (p colored-point))
  (list 'colored (slot-ref p 'color) (next-method)))

(describe (make colored-point #:x 3 #:color 'red))
; => (colored red (point 3))
```

`define-generic` also supports setter generics with `(setter name)` and fallback
procedures. `define-method` requires all specialized arguments to form a prefix
of the argument list; trailing unspecialized and rest arguments are allowed.

### Invocable classes

Classes marked applicable allocate procedure wrappers instead of returning the
raw instance directly. They must define a `procedure` slot containing a callable
value. The wrapper keeps the underlying instance and class in procedure
properties:

```scheme
(define-class command ()
  (procedure name)
  #:applicable #t)

(define run
  (make command
    #:name 'hello
    #:procedure (lambda (self arg)
                  (list (slot-ref self 'name) arg))))

(run 'world) ; => (hello world)
(procedure-property run 'instance) ; raw Scheme instance
```

### Developer constraints and pitfalls

- Built-in classes cannot be unsealed. `class-unseal!` only succeeds for Scheme
  classes.
- Dynamic Scheme classes are malleable until sealed; sealed classes reject
  redefinition.
- `slot-ref` without a fallback routes unbound slots through `slot-unbound` and
  missing slots through `slot-missing`. Supplying a fallback bypasses the missing
  slot generic.
- `class-slot-ref`, `class-slot-set!`, and `class-slot-bound?` are only valid
  for class-allocated slots.
- `change-class` only targets Scheme classes; built-in target classes are
  rejected.
- Applicable classes must include a `procedure` slot, and each applicable
  instance must initialize it with a procedure.
- The object header has no room for ids above `MAX_CLASS_ID`; exhausting class
  ids is treated as a fatal registry error.

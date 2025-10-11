# Implementation

CapyScheme aims to implement R6RS/R7RS specification and also be performant. Implementation of language
consists of "core expander",CPS compiler, runtime, and GC.

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
        letk (h) check_pair_typecode () =  {
            let tc8 = #% %typecode8 (h, x) in
            let pair_typecode = #% u8= (h, tc8, Value::new(TypeCode8::PAIR.bits())) in
            if pair_typecode => k | not_pair
        } in

        let is_immediate = #% immediate? (h, x) @ src;
        if is_immediate => not_pair | check_pair_typecode
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

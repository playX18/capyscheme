;; CFG / SSA-ish intermediate representation.

(library (core compiler cfg ir)
  (export
    ;; code ids
    graph-code-id case-group-id graph-code-id? case-group-id? code-id-number code-id=?
    raise-kind->index
    ;; program / procedure / block
    make-program program? program-entry program-procedures program-case-groups
    make-case-group case-group? case-group-code case-group-clauses
    make-cfg-proc cfg-proc? cfg-proc-code cfg-proc-binding cfg-proc-name cfg-proc-source cfg-proc-meta cfg-proc-params
    cfg-proc-variadic cfg-proc-free-vars cfg-proc-env-shared? cfg-proc-sources cfg-proc-entry
    cfg-proc-blocks cfg-proc-blocks-set!
    make-block block? block-id block-instructions block-instructions-set!
    block-terminator block-terminator-set! block-source
    block-uses-local?
    ;; operands
    make-operand-constant operand-constant? operand-constant-value
    apply-call-marker apply-call-callee?
    make-operand-local operand-local? operand-local-uvar
    operand=?
    ;; instructions
    make-insn-assign insn-assign? insn-assign-dst insn-assign-src
    make-insn-const insn-const? insn-const-dst insn-const-value
    make-insn-make-closure insn-make-closure? insn-make-closure-dst insn-make-closure-code
    insn-make-closure-free-count insn-make-closure-env
    make-insn-make-case-closure insn-make-case-closure? insn-make-case-closure-dst
    insn-make-case-closure-code insn-make-case-closure-clauses
    make-insn-make-env insn-make-env? insn-make-env-dst insn-make-env-size
    make-insn-env-ref insn-env-ref? insn-env-ref-dst insn-env-ref-env insn-env-ref-index
    make-insn-env-set insn-env-set? insn-env-set-env insn-env-set-index insn-env-set-value
    make-insn-closure-ref insn-closure-ref? insn-closure-ref-dst insn-closure-ref-closure
    insn-closure-ref-index
    make-insn-closure-set insn-closure-set? insn-closure-set-closure insn-closure-set-index
    insn-closure-set-value
    make-insn-cache-ref insn-cache-ref? insn-cache-ref-dst insn-cache-ref-cache-key
    insn-cache-ref-source
    make-insn-cache-set insn-cache-set? insn-cache-set-dst insn-cache-set-cache-key
    insn-cache-set-value insn-cache-set-source
    make-insn-prim-call insn-prim-call? insn-prim-call-dst insn-prim-call-prim
    insn-prim-call-args insn-prim-call-source
    make-insn-rest-to-list insn-rest-to-list? insn-rest-to-list-dst insn-rest-to-list-rest
    insn-rest-to-list-source
    make-insn-rest-ref insn-rest-ref? insn-rest-ref-dst insn-rest-ref-rest
    insn-rest-ref-index insn-rest-ref-source
    make-insn-rest-length insn-rest-length? insn-rest-length-dst insn-rest-length-rest
    insn-rest-length-skip insn-rest-length-source
    make-insn-rest-predicate insn-rest-predicate? insn-rest-predicate-dst
    insn-rest-predicate-rest insn-rest-predicate-predicate insn-rest-predicate-skip
    insn-rest-predicate-source
    make-insn-receive insn-receive? insn-receive-dst insn-receive-index
    make-insn-receive-rest insn-receive-rest? insn-receive-rest-dst insn-receive-rest-from
    insn-def insn-defs insn-uses
    ;; branch targets / switch
    make-return-point return-point? return-point-block return-point-nvals
    return-point-variadic?
    make-target-local target-local? target-local-block target-local-edge-assigns
    make-target-return target-return? target-return-args
    target-local-successor target-uses
    make-switch-case switch-case? switch-case-value switch-case-target
    make-case-integer case-integer? case-integer-value
    make-case-symbol case-symbol? case-symbol-hash case-symbol-value
    switch-case-value=?
    ;; terminators
    make-term-call term-call? term-call-callee term-call-ret term-call-args term-call-source
    make-term-tail-call term-tail-call? term-tail-call-callee term-tail-call-args
    term-tail-call-source
    make-term-return term-return? term-return-args term-return-source
    make-term-raise term-raise? term-raise-kind term-raise-args term-raise-source
    make-term-jump term-jump? term-jump-target
    make-term-branch term-branch? term-branch-test term-branch-consequent
    term-branch-alternative term-branch-hints
    make-term-branch-prim term-branch-prim? term-branch-prim-prim term-branch-prim-args
    term-branch-prim-consequent term-branch-prim-alternative term-branch-prim-hints
    make-term-switch term-switch? term-switch-kind term-switch-scrutinee term-switch-cases
    term-switch-default
    term-terminal? term-uses term-successors)
  (import (core) (capy) (only (core compiler cps graph) apply-call-marker))

  ;; Whether CALLEE is the marker callee of `(apply f a ... lst)'.
  (define (apply-call-callee? callee)
    (and (operand-constant? callee)
      (eq? (operand-constant-value callee) apply-call-marker)))

  (define (graph-code-id n) (cons 'gf n))
  (define (case-group-id n) (cons 'cg n))
  (define (graph-code-id? x) (and (pair? x) (eq? (car x) 'gf)))
  (define (case-group-id? x) (and (pair? x) (eq? (car x) 'cg)))
  (define (code-id-number x) (cdr x))
  (define (code-id=? a b)
    (and (eq? (car a) (car b)) (eqv? (cdr a) (cdr b))))

  (define (raise-kind->index kind)
    (case kind
      [(AssertionViolation) 0]
      [(WrongNumberOfArguments) 1]
      [(NonApplicable) 2]
      [(Undefined) 3]
      [(WrongNumberOfArgumentsCar) 4]
      [(WrongNumberOfArgumentsCdr) 5]
      [(WrongNumberOfArgumentsSetCar) 6]
      [(WrongNumberOfArgumentsSetCdr) 7]
      [(WrongNumberOfArgumentsVariableRef) 8]
      [(WrongNumberOfArgumentsVariableSet) 9]
      [(WrongNumberOfArgumentsVariableBound) 10]
      [(CarNotPair) 11]
      [(CdrNotPair) 12]
      [(FixnumOverflow) 13]
      [else (assertion-violation 'raise-kind->index "unknown raise kind" kind)]))

  (define-record-type program (fields entry procedures case-groups))
  (define-record-type case-group (fields code clauses))
  (define-record-type cfg-proc
    (fields code binding name source meta params variadic free-vars env-shared? sources entry
      (mutable blocks)))
  (define-record-type block
    (fields id (mutable instructions) (mutable terminator) source))

  (define-record-type operand-constant (fields value))
  (define-record-type operand-local (fields uvar))

  (define (operand=? a b)
    (cond
      [(operand-local? a)
       (and (operand-local? b) (eqv? (operand-local-uvar a) (operand-local-uvar b)))]
      [else
       (and (operand-constant? b)
         (eq? (operand-constant-value a) (operand-constant-value b)))]))

  (define-record-type insn-assign (fields dst src))
  (define-record-type insn-const (fields dst value))
  (define-record-type insn-make-closure (fields dst code free-count env))
  (define-record-type insn-make-case-closure (fields dst code clauses))
  (define-record-type insn-make-env (fields dst size))
  (define-record-type insn-env-ref (fields dst env index))
  (define-record-type insn-env-set (fields env index value))
  (define-record-type insn-closure-ref (fields dst closure index))
  (define-record-type insn-closure-set (fields closure index value))
  (define-record-type insn-cache-ref (fields dst cache-key source))
  (define-record-type insn-cache-set (fields dst cache-key value source))
  (define-record-type insn-prim-call (fields dst prim args source))
  (define-record-type insn-rest-to-list (fields dst rest source))
  (define-record-type insn-rest-ref (fields dst rest index source))
  (define-record-type insn-rest-length (fields dst rest skip source))
  (define-record-type insn-rest-predicate (fields dst rest predicate skip source))
  (define-record-type insn-receive (fields dst index))
  (define-record-type insn-receive-rest (fields dst from))

  (define-record-type return-point (fields block nvals variadic?))
  (define-record-type target-local (fields block edge-assigns))
  (define-record-type target-return (fields args))
  (define-record-type switch-case (fields value target))
  (define-record-type case-integer (fields value))
  (define-record-type case-symbol (fields hash value))

  (define (switch-case-value=? a b)
    (cond
      [(case-integer? a)
       (and (case-integer? b) (eqv? (case-integer-value a) (case-integer-value b)))]
      [else
       (and (case-symbol? b)
         (eqv? (case-symbol-hash a) (case-symbol-hash b))
         (eq? (case-symbol-value a) (case-symbol-value b)))]))

  (define-record-type term-call (fields callee ret args source))
  (define-record-type term-tail-call (fields callee args source))
  (define-record-type term-return (fields args source))
  (define-record-type term-raise (fields kind args source))
  (define-record-type term-jump (fields target))
  (define-record-type term-branch (fields test consequent alternative hints))
  (define-record-type term-branch-prim (fields prim args consequent alternative hints))
  (define-record-type term-switch (fields kind scrutinee cases default))

  (define (insn-def i)
    (cond
      [(insn-assign? i) (insn-assign-dst i)]
      [(insn-const? i) (insn-const-dst i)]
      [(insn-make-closure? i) (insn-make-closure-dst i)]
      [(insn-make-case-closure? i) (insn-make-case-closure-dst i)]
      [(insn-make-env? i) (insn-make-env-dst i)]
      [(insn-env-ref? i) (insn-env-ref-dst i)]
      [(insn-closure-ref? i) (insn-closure-ref-dst i)]
      [(insn-cache-ref? i) (insn-cache-ref-dst i)]
      [(insn-cache-set? i) (insn-cache-set-dst i)]
      [(insn-prim-call? i) (insn-prim-call-dst i)]
      [(insn-rest-to-list? i) (insn-rest-to-list-dst i)]
      [(insn-rest-ref? i) (insn-rest-ref-dst i)]
      [(insn-rest-length? i) (insn-rest-length-dst i)]
      [(insn-rest-predicate? i) (insn-rest-predicate-dst i)]
      [(insn-receive? i) (insn-receive-dst i)]
      [(insn-receive-rest? i) (insn-receive-rest-dst i)]
      [else #f]))   ; closure-set, env-set

  (define (insn-defs i)
    (let ([d (insn-def i)]) (if d (list d) '())))

  (define (insn-uses i)
    (cond
      [(insn-assign? i) (list (insn-assign-src i))]
      [(insn-const? i) '()]
      [(insn-make-closure? i)
       (let ([env (insn-make-closure-env i)]) (if env (list env) '()))]
      [(insn-make-case-closure? i) (insn-make-case-closure-clauses i)]
      [(insn-make-env? i) '()]
      [(insn-env-ref? i) (list (insn-env-ref-env i))]
      [(insn-env-set? i) (list (insn-env-set-env i) (insn-env-set-value i))]
      [(insn-closure-ref? i) (list (insn-closure-ref-closure i))]
      [(insn-closure-set? i) (list (insn-closure-set-closure i) (insn-closure-set-value i))]
      [(insn-cache-ref? i) (list (insn-cache-ref-cache-key i))]
      [(insn-cache-set? i) (list (insn-cache-set-cache-key i) (insn-cache-set-value i))]
      [(insn-prim-call? i) (insn-prim-call-args i)]
      [(insn-rest-to-list? i) (list (make-operand-local (insn-rest-to-list-rest i)))]
      [(insn-rest-ref? i) (list (make-operand-local (insn-rest-ref-rest i)))]
      [(insn-rest-length? i) (list (make-operand-local (insn-rest-length-rest i)))]
      [(insn-rest-predicate? i) (list (make-operand-local (insn-rest-predicate-rest i)))]
      [else '()]))  ; receive, receive-rest

  (define (target-local-successor t)
    (if (target-local? t) (target-local-block t) #f))

  (define (target-uses t)
    (if (target-local? t)
      (let loop ([is (target-local-edge-assigns t)] [acc '()])
        (if (null? is)
          (reverse acc)
          (loop (cdr is) (append (reverse (insn-uses (car is))) acc))))
      (target-return-args t)))


  (define (term-terminal? t) (term-raise? t))

  (define (term-uses t)
    (cond
      [(term-call? t) (cons (term-call-callee t) (term-call-args t))]
      [(term-tail-call? t) (cons (term-tail-call-callee t) (term-tail-call-args t))]
      [(term-raise? t) (term-raise-args t)]
      [(term-return? t) (term-return-args t)]
      [(term-jump? t) '()]
      [(term-branch? t)
       (cons (term-branch-test t)
         (append (target-uses (term-branch-consequent t))
           (target-uses (term-branch-alternative t))))]
      [(term-branch-prim? t)
       (append (term-branch-prim-args t)
         (target-uses (term-branch-prim-consequent t))
         (target-uses (term-branch-prim-alternative t)))]
      [else
       (cons (term-switch-scrutinee t)
         (append
           (let loop ([cs (term-switch-cases t)] [acc '()])
             (if (null? cs)
               (reverse acc)
               (loop (cdr cs)
                 (append (reverse (target-uses (switch-case-target (car cs)))) acc))))
           (target-uses (term-switch-default t))))]))

  (define (local-successors . targets)
    (let loop ([ts targets] [acc '()])
      (if (null? ts)
        (reverse acc)
        (let ([s (target-local-successor (car ts))])
          (loop (cdr ts) (if s (cons s acc) acc))))))

  (define (term-successors t)
    (cond
      [(term-call? t) (list (return-point-block (term-call-ret t)))]
      [(or (term-tail-call? t) (term-raise? t) (term-return? t)) '()]
      [(term-jump? t) (list (term-jump-target t))]
      [(term-branch? t)
       (local-successors (term-branch-consequent t) (term-branch-alternative t))]
      [(term-branch-prim? t)
       (local-successors (term-branch-prim-consequent t) (term-branch-prim-alternative t))]
      [else
       (apply local-successors
         (append (map switch-case-target (term-switch-cases t))
           (list (term-switch-default t))))]))

  ;; Whether `uvar' appears as a use in this block's body or terminator.
  (define (block-uses-local? b uvar)
    (define (use? o) (and (operand-local? o) (eqv? (operand-local-uvar o) uvar)))
    (or (exists (lambda (i) (exists use? (insn-uses i))) (block-instructions b))
      (exists use? (term-uses (block-terminator b)))))
)

;; GCPS graph -> CFG lowering.

(library (core compiler cfg lower)
  (export lower-graph)
  (import (core) (capy)
    (core compiler cps graph)
    (core compiler cps reify)
    (core compiler cfg ir)
    (core compiler cfg finish))

  ;; Left-to-right map (uvar/block numbering must be deterministic).
  (define (map-ordered f l)
    (let loop ([l l] [acc '()])
      (if (null? l) (reverse acc) (loop (cdr l) (cons (f (car l)) acc)))))

  (define (graph-code-id-of g function)
    (unless (function-cont (graph-function g function))
      (error 'lower-graph "continuations are not procedures"))
    (graph-code-id function))

  (define (lower-graph g reify share)
    (let ([case-groups '()]            ; reversed
          [case-group-count 0])
      (define (add-case-group! clauses)
        (let ([code (case-group-id case-group-count)])
          (set! case-groups (cons (make-case-group code clauses) case-groups))
          (set! case-group-count (+ case-group-count 1))
          code))
      (let ([procedures
             (map-ordered
               (lambda (function)
                 (finish-procedure (lower-function g reify function add-case-group!)))
               (reify-info-functions reify))])
        (make-program (graph-code-id-of g (reify-info-entrypoint reify))
          procedures
          (reverse case-groups)))))

  ;; Whether VAR is the function's own binding (the closure's self reference).
  (define (self-var? g function var)
    (eq? (bound-var-data-var (graph-bound-var g var))
         (bound-var-data-var (graph-bound-var g (function-var (graph-function g function))))))

  (define (physical-free-vars g function vars)
    (filter (lambda (v) (not (self-var? g function v))) vars))

  (define (contified-arity-ok? formals variadic args)
    (if variadic
      (>= (length args) (length formals))
      (= (length args) (length formals))))

  (define (list-head l n)
    (if (or (= n 0) (null? l)) '() (cons (car l) (list-head (cdr l) (- n 1)))))

  (define (list-drop l n)
    (if (or (= n 0) (null? l)) l (list-drop (cdr l) (- n 1))))

  (define (lower-function g reify function add-case-group!)
    (let* ([data (graph-function g function)]
           [return-cont (or (function-cont data)
                          (error 'lower-graph "graph function should have a return continuation"))]
           [fv (reify-info-free-vars reify)]
           [source-free-vars (entity-set->list (reify-free-vars-function fv function))]
           [values-table (make-eqv-hashtable)]       ; BoundVar -> UVar
           [sources '()]                              ; reversed alist
           [blocks '()]                               ; reversed, sorted at the end
           [local-blocks (make-eqv-hashtable)]        ; BoundVar -> BlockId
           [local-formals (make-eqv-hashtable)]       ; BoundVar -> (formals . variadic)
           [known-literals (make-eqv-hashtable)]
           [case-clause-groups (make-eqv-hashtable)]  ; UVar -> CodeId
           [next-uvar 0]
           [next-block 1])

      (define (uvar var)
        (or (hashtable-ref values-table var #f)
          (let ([id next-uvar])
            (set! next-uvar (+ next-uvar 1))
            (hashtable-set! values-table var id)
            (set! sources
              (cons (cons id (lvar-name (bound-var-data-var (graph-bound-var g var)))) sources))
            id)))

      (define (fresh-temp)
        (let ([id next-uvar])
          (set! next-uvar (+ next-uvar 1))
          id))

      (define (uvars vars) (map-ordered uvar vars))

      (define (atom var) (make-operand-local (uvar (free-binder g var))))
      (define (atoms vars) (map-ordered atom (free-vars-slice g vars)))

      (define (literal-atom var)
        (let ([v (hashtable-ref known-literals (free-binder g var) known-literals)])
          (if (eq? v known-literals) #f (make-operand-constant v))))

      (define (atoms-for-prim prim vars)
        (let* ([args (atoms vars)]
               [slice (free-vars-slice g vars)])
          (cond
            [(and (memq prim '(cache-ref cache-set!)) (pair? slice) (literal-atom (car slice)))
              => (lambda (lit) (cons lit (cdr args)))]
            [(and (memq prim '(slot-ref slot-set!)) (pair? slice) (pair? (cdr slice))
                  (literal-atom (cadr slice)))
              => (lambda (lit) (cons (car args) (cons lit (cddr args))))]
            [else args])))

      (define (alloc-block)
        (let ([id next-block])
          (set! next-block (+ next-block 1))
          id))

      (define (push-block! id instructions terminator source)
        (set! blocks (cons (make-block id instructions terminator source) blocks)))

      ;; Instruction accumulators: a one-element vector holding the reversed list.
      (define (make-acc initial) (vector (reverse initial)))
      (define (emit! acc insn) (vector-set! acc 0 (cons insn (vector-ref acc 0))))
      (define (acc->list acc) (reverse (vector-ref acc 0)))

      (define (assert-return-cont continuation)
        (unless (eqv? continuation return-cont)
          (error 'lower-graph
            "continuation is neither a block of this function nor its return continuation; continuations are second-class"
            continuation)))

      (define (formals-of continuation)
        (hashtable-ref local-formals continuation (cons '() #f)))

      ;; Parallel destinations <- sources via temp staging (handles cycles).
      (define (emit-parallel-moves! acc destinations sources)
        (unless (null? destinations)
          (let* ([ndest (length destinations)]
                 [srcs (let ([s (if (> (length sources) ndest)
                                  (list-head sources ndest)
                                  sources)])
                         (append s
                           (let loop ([k (- ndest (length s))] [acc '()])
                             (if (<= k 0) acc (loop (- k 1) (cons (make-operand-constant '()) acc))))))]
                 [temps (map-ordered
                          (lambda (src)
                            (if (operand-local? src)
                              (operand-local-uvar src)
                              (let ([temp (fresh-temp)])
                                (emit! acc (make-insn-assign temp src))
                                temp)))
                          srcs)]
                 [staged (list->vector temps)])
            (let loop ([i 0] [ss srcs])
              (unless (null? ss)
                (let ([src (car ss)])
                  (when (and (operand-local? src)
                             (memv (operand-local-uvar src) destinations))
                    (let ([temp (fresh-temp)])
                      (emit! acc (make-insn-assign temp src))
                      (vector-set! staged i temp))))
                (loop (+ i 1) (cdr ss))))
            (let loop ([ds destinations] [i 0])
              (unless (null? ds)
                (let ([src (vector-ref staged i)])
                  (unless (eqv? (car ds) src)
                    (emit! acc (make-insn-assign (car ds) (make-operand-local src)))))
                (loop (cdr ds) (+ i 1)))))))

      (define (emit-parallel-assign! acc formals variadic args)
        (let ([fixed-count (length formals)])
          (emit-parallel-moves! acc formals (list-head args fixed-count))
          (when variadic
            ;; Right-fold cons over the excess arguments into `rest'.
            (let loop ([rest-args (reverse (list-drop args fixed-count))]
                       [lst (make-operand-constant '())])
              (if (null? rest-args)
                (emit! acc (make-insn-assign variadic lst))
                (let ([dst (fresh-temp)])
                  (emit! acc (make-insn-prim-call dst 'cons (list (car rest-args) lst) #f))
                  (loop (cdr rest-args) (make-operand-local dst))))))))

      (define (emit-closure-sets! acc closure free-vars)
        (let loop ([vs free-vars] [index 0])
          (unless (null? vs)
            (emit! acc (make-insn-closure-set (make-operand-local closure) index
                         (make-operand-local (uvar (car vs)))))
            (loop (cdr vs) (+ index 1)))))

      ;; Build the return-point block for a call whose continuation is the
      ;; local continuation CONTINUATION (block TARGET).
      (define (return-point continuation target)
        (let* ([fm (formals-of continuation)]
               [formals (car fm)]
               [variadic (cdr fm)]
               [block (alloc-block)]
               [insns (let loop ([fs formals] [i 0] [acc '()])
                        (if (null? fs)
                          (reverse acc)
                          (loop (cdr fs) (+ i 1)
                            (cons (make-insn-receive (car fs) i) acc))))]
               [insns (if variadic
                        (append insns (list (make-insn-receive-rest variadic (length formals))))
                        insns)])
          (push-block! block insns (make-term-jump target) #f)
          (make-return-point block (length formals) (and variadic #t))))

      (define (continuation-target continuation-var args)
        (let* ([continuation (free-binder g continuation-var)]
               [block (hashtable-ref local-blocks continuation #f)])
          (if block
            (let* ([helper (alloc-block)]
                   [fm (formals-of continuation)]
                   [formals (car fm)]
                   [variadic (cdr fm)])
              (if (not (contified-arity-ok? formals variadic args))
                (push-block! helper '()
                  (make-term-raise 'AssertionViolation '() #f) #f)
                (let ([acc (make-acc '())])
                  (emit-parallel-assign! acc formals variadic args)
                  (push-block! helper (acc->list acc) (make-term-jump block) #f)))
              (make-target-local helper '()))
            (begin
              (assert-return-cont continuation)
              (make-target-return args)))))

      (define (branch-target link)
        (let ([dc (direct-continue g link)])
          (if dc
            (let ([args (atoms (cdr dc))])
              (continuation-target (car dc) args))
            (let ([block (alloc-block)])
              (convert-block block '() link)
              (make-target-local block '())))))

      (define (convert-expr! binding expr acc)
        (let* ([e (graph-expr g expr)]
               [kind (expr-kind e)])
          (if (k-literal? kind)
            (let ([dst (uvar binding)] [value (k-literal-value kind)])
              (hashtable-set! known-literals binding value)
              (emit! acc (make-insn-const dst value)))
            (let* ([prim (k-prim-call-prim kind)]
                   [args (atoms-for-prim prim (k-prim-call-vars kind))]
                   [dst (uvar binding)])
              (if (eq? prim '%case-lambda)
                (let ([code (let loop ([as args])
                              (cond
                                [(null? as)
                                  (error 'lower-graph
                                    "case-lambda clause closure was not created at this site")]
                                [(and (operand-local? (car as))
                                      (hashtable-ref case-clause-groups
                                        (operand-local-uvar (car as)) #f))
                                  => (lambda (c) c)]
                                [else (loop (cdr as))]))])
                  (emit! acc (make-insn-make-case-closure dst code args)))
                (emit! acc (make-insn-prim-call dst prim args (expr-source e))))))))

      (define (convert-block id instructions link)
        (let ([term (or (read-term-link g link)
                      (error 'lower-graph "dead graph term link while lowering to CFG" link))])
          (let* ([source (term-source (graph-term g term))]
                 [acc (make-acc instructions)]
                 [terminator (convert-term term acc)])
            (push-block! id (acc->list acc) terminator source))))

      (define (convert-term-link link acc)
        (let ([term (or (read-term-link g link)
                      (error 'lower-graph "dead graph term link while lowering to CFG" link))])
          (convert-term term acc)))

      ;; Emit the closure creations of the live functions FUNCTIONS paired with
      ;; code ids (list of (function . code)).
      (define (emit-closures! acc plan)
        (for-each
          (lambda (p)
            (let ([function (car p)])
              (emit! acc (make-insn-make-closure
                           (uvar (function-var (graph-function g function)))
                           (cdr p)
                           (length (physical-free-vars g function
                                     (entity-set->list (reify-free-vars-function fv function))))
                           #f))))
          plan)
        (for-each
          (lambda (p)
            (let ([function (car p)])
              (emit-closure-sets! acc (uvar (function-var (graph-function g function)))
                (physical-free-vars g function
                  (entity-set->list (reify-free-vars-function fv function))))))
          plan))

      (define (convert-term term acc)
        (let* ([data (graph-term g term)]
               [kind (term-kind data)]
               [source (term-source data)])
          (cond
            [(k-let-val? kind)
              (let ([expr (or (read-expr-link g (k-let-val-expr kind))
                            (error 'lower-graph "dead graph expression link while lowering to CFG"))])
                (convert-expr! (k-let-val-binder kind) expr acc)
                (convert-term-link (k-let-val-body kind) acc))]

            [(k-fix? kind)
              (let* ([functions (live-functions g (function-links-slice g (k-fix-functions kind)))]
                     ;; case groups in first-seen order: list of (id . members)
                     [groups
                       (let loop ([fs functions] [groups '()])
                         (if (null? fs)
                           (map (lambda (grp) (cons (car grp) (reverse (cdr grp)))) (reverse groups))
                           (let ([cg (function-case-group (graph-function g (car fs)))])
                             (cond
                               [(not cg) (loop (cdr fs) groups)]
                               [(assv cg groups)
                                 => (lambda (entry)
                                      (set-cdr! entry (cons (car fs) (cdr entry)))
                                      (loop (cdr fs) groups))]
                               [else (loop (cdr fs) (cons (list cg (car fs)) groups))]))))])
                (if (null? groups)
                  (emit-closures! acc
                    (map (lambda (f) (cons f (graph-code-id-of g f))) functions))
                  (let ([plan '()])      ; reversed list of (function . code)
                    (for-each
                      (lambda (grp)
                        (let* ([members (cdr grp)]
                               [code (add-case-group!
                                       (map (lambda (f) (graph-code-id-of g f)) members))])
                          (for-each
                            (lambda (f)
                              (hashtable-set! case-clause-groups
                                (uvar (function-var (graph-function g f))) code)
                              (set! plan (cons (cons f code) plan)))
                            members)))
                      groups)
                    (for-each
                      (lambda (f)
                        (unless (function-case-group (graph-function g f))
                          (set! plan (cons (cons f (graph-code-id-of g f)) plan))))
                      functions)
                    (emit-closures! acc (reverse plan))))
                (convert-term-link (k-fix-body kind) acc))]

            [(k-letk? kind)
              ;; Every continuation is a block of this function.
              (let ([local-conts (live-functions g (function-links-slice g (k-letk-functions kind)))])
                (for-each
                  (lambda (continuation)
                    (let* ([id (alloc-block)]
                           [cdata (graph-function g continuation)])
                      (hashtable-set! local-blocks (function-var cdata) id)
                      (let* ([formals (uvars (bound-vars-slice g (function-vars cdata)))]
                             [variadic (and (function-variadic cdata)
                                            (uvar (function-variadic cdata)))])
                        (hashtable-set! local-formals (function-var cdata)
                          (cons formals variadic)))))
                  local-conts)
                (for-each
                  (lambda (continuation)
                    (let ([cdata (graph-function g continuation)])
                      (convert-block (hashtable-ref local-blocks (function-var cdata) #f)
                        '() (function-body cdata))))
                  local-conts)
                (convert-term-link (k-letk-body kind) acc))]

            [(k-continue? kind)
              (let* ([target (free-binder g (k-continue-cont kind))]
                     [block (hashtable-ref local-blocks target #f)])
                (if block
                  (let* ([args (atoms (k-continue-vars kind))]
                         [fm (formals-of target)]
                         [formals (car fm)]
                         [variadic (cdr fm)])
                    (if (not (contified-arity-ok? formals variadic args))
                      (make-term-raise 'AssertionViolation '() source)
                      (begin
                        (emit-parallel-assign! acc formals variadic args)
                        (make-term-jump block))))
                  (begin
                    (assert-return-cont target)
                    (make-term-return (atoms (k-continue-vars kind)) source))))]

            [(k-app? kind)
              (let* ([retk (free-binder g (k-app-cont kind))]
                     [callee (let ([lit (literal-atom (k-app-func kind))])
                               (if (and lit (eq? (operand-constant-value lit) apply-call-marker))
                                 lit
                                 (atom (k-app-func kind))))]
                     [args (atoms (k-app-vars kind))]
                     [target (hashtable-ref local-blocks retk #f)])
                (if target
                  (let ([ret (return-point retk target)])
                    (make-term-call callee ret args source))
                  (begin
                    (assert-return-cont retk)
                    (make-term-tail-call callee args source))))]

            [(k-raise? kind)
              (make-term-raise (k-raise-kind kind) (atoms (k-raise-vars kind)) source)]

            [(k-if? kind)
              (let* ([test (atom (k-if-test kind))]
                     [consequent (branch-target (k-if-then kind))]
                     [alternative (branch-target (k-if-else kind))]
                     [hints (k-if-hints kind)])
                (make-term-branch test consequent alternative (list (car hints) (cadr hints))))]

            [else                       ; branch-prim
              (let ([op (k-branch-prim-op kind)])
                (let-values ([(args true-args false-args)
                              (branch-operands g op (k-branch-prim-vars kind))])
                  (let* ([args (map-ordered atom args)]
                         [true-args (map-ordered atom true-args)]
                         [false-args (map-ordered atom false-args)]
                         [consequent (continuation-target (k-branch-prim-consequent kind) true-args)]
                         [alternative (continuation-target (k-branch-prim-alternative kind) false-args)])
                    (make-term-branch-prim (branch-op-prim op) args consequent alternative
                      (branch-op-hints op)))))])))

      ;; Materialize a flat closure's captures at function entry.  The self
      ;; reference is materialized last through a dummy slot index (codegen
      ;; binds `rator', so no slot).
      (for-each
        (lambda (free-var)
          (when (reify-free-vars-continuation-of-binder fv free-var)
            (error 'lower-graph
              "function captures a continuation; continuations are second-class"
              (function-name data))))
        source-free-vars)

      (let* ([binding (uvar (function-var data))]
             [params (uvars (bound-vars-slice g (function-vars data)))]
             [variadic (and (function-variadic data) (uvar (function-variadic data)))])
        (let loop ([vs source-free-vars] [insns '()] [physical '()] [self-uvar #f])
          (cond
            [(pair? vs)
              (let ([u (uvar (car vs))])
                (if (self-var? g function (car vs))
                  (loop (cdr vs) insns physical u)
                  (loop (cdr vs)
                    (cons (make-insn-closure-ref u (make-operand-local binding) (length physical))
                      insns)
                    (cons u physical)
                    self-uvar)))]
            [else
              (let* ([insns (if self-uvar
                              (cons (make-insn-closure-ref self-uvar (make-operand-local binding)
                                      (length physical))
                                insns)
                              insns)]
                     [physical (if self-uvar (cons self-uvar physical) physical)]
                     [entry 0])
                (convert-block entry (reverse insns) (function-body data))
                (make-cfg-proc (graph-code-id-of g function)
                  binding
                  (function-name data)
                  (function-source data)
                  (function-meta data)
                  params
                  variadic
                  (reverse physical)
                  #f
                  (reverse sources)
                  entry
                  (list-sort (lambda (a b) (< (block-id a) (block-id b))) (reverse blocks))))])))))
)

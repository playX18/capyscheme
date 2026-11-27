;; The mutable graph CPS: entity arenas over terms, exprs and functions.
;;
;; Port of crates/capy/src/compiler/cps/graph.rs.
;;
;; Representation (mirrors cranelift_entity::PrimaryMap):
;;
;;   * Every entity kind (term links, expr links, parent links, function
;;     links, free occurrences, union-find points, bound vars, terms, exprs,
;;     functions) lives in a growable vector owned by the <graph>.  An
;;     entity id is the fixnum index of its slot; ids are handed out in
;;     creation order, so numbering is deterministic.
;;   * `Option<Id>` is the id or #f.  `Option<BoundVar>` and friends likewise.
;;   * `EntityList<T>` (FreeVars, BoundVars, FunctionLinks) is an immutable
;;     Scheme list of ids.  `free-vars-slice` & co. return that list itself.
;;     "Pushing" returns a new list: (free-vars-push vars var) => vars'.
;;   * `Term`, `Expr`, `Function`, `BoundVarData`, `FreeOccPayload`, `UfNode`
;;     are mutable records.  They are REFERENCES, not copies: unlike Rust's
;;     `let data = graph[f];` (a Copy), holding a record and then mutating
;;     the graph is visible through it.  Use `function-copy`, `term-copy`,
;;     `expr-copy` where a snapshot is required.
;;   * `TermKind` is a set of record types, one per variant (k-let-val, k-fix,
;;     k-letk, k-if, k-branch-prim, k-continue, k-app, k-raise), each with a
;;     match pattern (~k-let-val binder expr body) for (core pattern-match).
;;     A term's kind is replaced wholesale with `term-kind-set!`.
;;   * `ExprKind` is k-literal / k-prim-call with patterns ~k-literal and
;;     ~k-prim-call.
;;   * `Parent` is a fixnum: (parent-term id) / (parent-func id), inspected with
;;     parent-term? parent-func? parent-id.
;;   * `BranchHint` is one of the symbols Normal Hot Cold; a hints pair is a
;;     two element list.  `RaiseKind` is the symbol of the Rust variant name
;;     (AssertionViolation, WrongNumberOfArguments, ...), see raise-kind->code.
;;   * `Primitive` (branch-prim's `prim`) is the primitive name symbol.
;;   * `Atom`: a constant is any Scheme value that is not an <lvar>; a local is
;;     an <lvar>.  See lvar? / atom-local?.
;;   * `ActiveLinkStatus`: the active term link id, or #f for TermIsDead.
;;
;; Exported API (Rust name -> Scheme name; snake_case -> kebab-case):
;;
;;   Local variables (LVarRef)
;;     make-lvar lvar? lvar-name lvar-id lvar-copy atom-local? atom-constant?
;;
;;   Branch hints / raise kinds
;;     branch-hint? raise-kind->code code->raise-kind raise-kind-name
;;
;;   Entity sets (cranelift EntitySet)
;;     make-entity-set entity-set-insert! entity-set-remove!
;;     entity-set-contains? entity-set-clear! entity-set->list entity-set-count
;;
;;   Parent
;;     parent-term parent-func parent-term? parent-func? parent-id
;;
;;   Term kinds (variants of TermKind)
;;     k-let-val  (make-k-let-val binder expr body)  accessors k-let-val-binder
;;                k-let-val-expr k-let-val-body       (Known<Subexpr> = binder + expr)
;;     k-fix      (make-k-fix functions body)         k-fix-functions k-fix-body
;;     k-letk     (make-k-letk functions body)        k-letk-functions k-letk-body
;;     k-if       (make-k-if test then else hints)    k-if-test k-if-then k-if-else k-if-hints
;;     k-branch-prim (make-k-branch-prim op consequent alternative vars)
;;                k-branch-prim-op -consequent -alternative -vars
;;     k-continue (make-k-continue cont vars)         k-continue-cont k-continue-vars
;;     k-app      (make-k-app func vars cont)         k-app-func k-app-vars k-app-cont
;;     k-raise    (make-k-raise kind vars)            k-raise-kind k-raise-vars
;;     predicates k-let-val? k-fix? k-letk? k-if? k-branch-prim? k-continue? k-app? k-raise?
;;     patterns   ~k-let-val ~k-fix ~k-letk ~k-if ~k-branch-prim ~k-continue ~k-app ~k-raise
;;     BranchOp   make-branch-op branch-op? branch-op-prim branch-op-nargs
;;                branch-op-ntrue branch-op-hints
;;
;;   Expr kinds
;;     k-literal (make-k-literal value) k-literal-value k-literal?
;;     k-prim-call (make-k-prim-call prim vars) k-prim-call-prim k-prim-call-vars k-prim-call?
;;     patterns ~k-literal ~k-prim-call
;;
;;   Node records
;;     Term:  make-cps-term cps-term? term-uplink term-kind term-source
;;            term-uplink-set! term-kind-set! term-source-set! term-copy
;;     Expr:  make-cps-expr cps-expr? expr-uplink expr-kind expr-source
;;            expr-uplink-set! expr-kind-set! expr-source-set! expr-copy
;;     Function: make-function function? function-name function-source function-var
;;            function-vars function-variadic function-cont function-is-variadic
;;            function-body function-is-rec function-is-cold function-is-noinline
;;            function-is-reified function-meta function-case-group, and for each
;;            field a setter function-<field>-set!; function-copy
;;     BoundVarData: bound-var-data-var bound-var-data-occ
;;     FreeOccPayload: free-occ-payload-point -owner -prev -next
;;     UfNode: uf-node-parent uf-node-descriptor
;;
;;   Graph (new-*: allocate and return the new id)
;;     make-graph graph? graph-stats
;;     graph-term graph-expr graph-function graph-bound-var graph-free-occ
;;     graph-point                                (Index<..> for every entity kind)
;;     graph-term-set! graph-expr-set! graph-function-set!   (IndexMut whole-slot)
;;     new-term-link read-term-link set-term-link! clear-term-link!
;;     new-expr-link read-expr-link set-expr-link! clear-expr-link!
;;     new-parent-link read-parent-link set-parent-link! clear-parent-link!
;;     new-function-link read-function-link set-function-link! clear-function-link!
;;     new-bound-var set-bound-var-occ! bound-var-occ binder-is-dead?
;;     new-point find-root find-root-const free-binder-rooted
;;     function-binder-rooted union-to! point-descriptor point-descriptor-const
;;     set-point-descriptor! points-equal?
;;     free-binder free-owner set-free-owner! new-free-occ free-occ-singleton
;;     free-occ-next free-occ-prev set-free-occ-next! set-free-occ-prev!
;;     free-occ-is-singleton? free-occ-append! free-occ-merge! free-occ-remove!
;;     remove-free-occ! free-occ->list for-each-occurrence collect-occurrences
;;     free-occ-is-singleton-verified? binder-is-singleton-or-dead?
;;     new-free-occ-for-binder merge-free-lists! subst-binders!
;;     subst-var-for-bound! subst-var-for-var! subst-var-for-var-points!
;;     subst-var-for-binders!
;;     new-free-vars free-vars-slice branch-operands (=> 3 values) free-vars-push
;;     clear-free-vars new-bound-vars bound-vars-slice bound-vars-push
;;     new-function-links function-links-slice live-functions direct-continue
;;     (=> (cont . args) or #f) function-links-push
;;     free-vars-of-expr push-free-vars-of-expr for-each-free-var-of-expr
;;     expr-free-vars-contain-any? direct-free-vars-of-term
;;     push-direct-free-vars-of-term for-each-direct-free-var-of-term
;;     subexprs-of for-each-subexpr subterms-of for-each-subterm
;;     get-active-link-for replace-active-subterm! backpatch-subterms!
;;     backpatch-subexprs! backpatch-function!
;;     new-term new-expr new-function exprs-iter (=> list of expr ids)
;;     term-links expr-links parent-links function-links   (=> arena vectors, see
;;     arena->list)
;;     display-term display-expr display-function pretty-term pretty-expr
;;     pretty-function (the latter are the debug renderers; for dump text use
;;     (core compiler cps pretty))
;;     entity-display-name
;;
;;   Misc: arena? arena-length arena-ref arena->list
(library (core compiler cps graph)
  (export
    make-lvar lvar? lvar-name lvar-id lvar-copy atom-local? atom-constant?
    apply-call-marker
    branch-hint? raise-kind->code code->raise-kind raise-kind-name
    make-entity-set entity-set-insert! entity-set-remove! entity-set-contains?
    entity-set-clear! entity-set->list entity-set-count entity-set-union! entity-set-copy
    parent-term parent-func parent-term? parent-func? parent-id
    make-k-let-val k-let-val? k-let-val-binder k-let-val-expr k-let-val-body
    make-k-fix k-fix? k-fix-functions k-fix-body
    make-k-letk k-letk? k-letk-functions k-letk-body
    make-k-if k-if? k-if-test k-if-then k-if-else k-if-hints
    make-k-branch-prim k-branch-prim? k-branch-prim-op k-branch-prim-consequent
    k-branch-prim-alternative k-branch-prim-vars
    make-k-continue k-continue? k-continue-cont k-continue-vars
    make-k-app k-app? k-app-func k-app-vars k-app-cont
    make-k-raise k-raise? k-raise-kind k-raise-vars
    ~k-let-val ~k-fix ~k-letk ~k-if ~k-branch-prim ~k-continue ~k-app ~k-raise
    make-branch-op branch-op? branch-op-prim branch-op-nargs branch-op-ntrue
    branch-op-hints
    make-k-literal k-literal? k-literal-value
    make-k-prim-call k-prim-call? k-prim-call-prim k-prim-call-vars
    ~k-literal ~k-prim-call
    make-cps-term cps-term? term-uplink term-kind term-source
    term-uplink-set! term-kind-set! term-source-set! term-copy
    make-cps-expr cps-expr? expr-uplink expr-kind expr-source
    expr-uplink-set! expr-kind-set! expr-source-set! expr-copy
    make-function function? function-copy
    function-name function-source function-var function-vars function-variadic
    function-cont function-is-variadic function-body function-is-rec
    function-is-cold function-is-noinline function-is-reified function-meta
    function-case-group
    function-name-set! function-source-set! function-var-set! function-vars-set!
    function-variadic-set! function-cont-set! function-is-variadic-set!
    function-body-set! function-is-rec-set! function-is-cold-set!
    function-is-noinline-set! function-is-reified-set! function-meta-set!
    function-case-group-set!
    bound-var-data-var bound-var-data-occ
    free-occ-payload-point free-occ-payload-owner free-occ-payload-prev
    free-occ-payload-next
    uf-node-parent uf-node-descriptor
    make-graph graph? graph-stats
    graph-term graph-expr graph-function graph-bound-var graph-free-occ graph-point
    graph-term-set! graph-expr-set! graph-function-set!
    new-term-link read-term-link set-term-link! clear-term-link!
    new-expr-link read-expr-link set-expr-link! clear-expr-link!
    new-parent-link read-parent-link set-parent-link! clear-parent-link!
    new-function-link read-function-link set-function-link! clear-function-link!
    new-bound-var set-bound-var-occ! bound-var-occ binder-is-dead?
    new-point find-root find-root-const free-binder-rooted function-binder-rooted
    union-to! point-descriptor point-descriptor-const set-point-descriptor!
    points-equal?
    free-binder free-owner set-free-owner! new-free-occ free-occ-singleton
    free-occ-next free-occ-prev set-free-occ-next! set-free-occ-prev!
    free-occ-is-singleton? free-occ-append! free-occ-merge! free-occ-remove!
    remove-free-occ! free-occ->list for-each-occurrence collect-occurrences
    free-occ-is-singleton-verified? binder-is-singleton-or-dead?
    new-free-occ-for-binder merge-free-lists! subst-binders!
    subst-var-for-bound! subst-var-for-var! subst-var-for-var-points!
    subst-var-for-binders!
    new-free-vars free-vars-slice branch-operands free-vars-push clear-free-vars
    new-bound-vars bound-vars-slice bound-vars-push
    new-function-links function-links-slice live-functions direct-continue
    function-links-push
    free-vars-of-expr push-free-vars-of-expr for-each-free-var-of-expr
    expr-free-vars-contain-any? direct-free-vars-of-term
    push-direct-free-vars-of-term for-each-direct-free-var-of-term
    subexprs-of for-each-subexpr subterms-of for-each-subterm
    get-active-link-for replace-active-subterm! backpatch-subterms!
    backpatch-subexprs! backpatch-function!
    new-term new-expr new-function exprs-iter
    term-links expr-links parent-links function-links
    display-term display-expr display-function pretty-term pretty-expr
    pretty-function entity-display-name
    arena? arena-length arena-ref arena->list)
  (import (core) (capy) (core pattern-match))

  ;; The literal callee of a call that is `(apply f a ... lst)': the call's
  ;; other operands are `f a ... lst'.  The bytecode lowering turns the call
  ;; into the `apply' / `tail-apply' instruction.  Compared with `eq?': no
  ;; program has this object as a literal.
  (define apply-call-marker (list 'apply-call))


  ;; A lexical variable.  Identity is the record itself (eq?).
  (define-record-type (lvar make-lvar lvar?)
    (fields name id))


  ;; LVar::copy: a fresh variable named NAME'.
  (define (lvar-copy v)
    (make-lvar
      (string->uninterned-symbol (format "~a'" (lvar-name v)))
      (lvar-id v)))

  (define (atom-local? a) (lvar? a))
  (define (atom-constant? a) (not (lvar? a)))

  (define (branch-hint? x) (and (memq x '(Normal Hot Cold)) #t))

  (define raise-kind-names
    '#(AssertionViolation WrongNumberOfArguments NonApplicable Undefined
        WrongNumberOfArgumentsCar WrongNumberOfArgumentsCdr
        WrongNumberOfArgumentsSetCar WrongNumberOfArgumentsSetCdr
        WrongNumberOfArgumentsVariableRef WrongNumberOfArgumentsVariableSet
        WrongNumberOfArgumentsVariableBound CarNotPair CdrNotPair FixnumOverflow))

  (define (raise-kind->code kind)
    (let loop ([i 0])
      (cond
        [(= i (vector-length raise-kind-names))
          (assertion-violation 'raise-kind->code "unknown raise kind" kind)]
        [(eq? (vector-ref raise-kind-names i) kind) i]
        [else (loop (+ i 1))])))

  (define (code->raise-kind code)
    (and (>= code 0) (< code (vector-length raise-kind-names))
      (vector-ref raise-kind-names code)))

  (define (raise-kind-name kind) (symbol->string kind))

  (define-record-type (arena make-arena arena?)
    (fields (mutable items arena-items arena-items-set!)
      (mutable len arena-length arena-len-set!)))

  (define (new-arena) (make-arena (make-vector 16 #f) 0))

  (define (arena-push! a x)
    (let ([n (arena-length a)] [items (arena-items a)])
      (when (= n (vector-length items))
        (let ([bigger (make-vector (* 2 n) #f)])
          (let loop ([i 0])
            (when (< i n)
              (vector-set! bigger i (vector-ref items i))
              (loop (+ i 1))))
          (arena-items-set! a bigger)))
      (vector-set! (arena-items a) n x)
      (arena-len-set! a (+ n 1))
      n))

  (define (arena-ref a i) (vector-ref (arena-items a) i))
  (define (arena-set! a i x) (vector-set! (arena-items a) i x))
  (define (arena-next-key a) (arena-length a))

  (define (arena->list a)
    (let loop ([i (- (arena-length a) 1)] [acc '()])
      (if (< i 0) acc (loop (- i 1) (cons (arena-ref a i) acc)))))

  ;; EntitySet: a set of non-negative ids, iterated in id order.
  ;;
  ;; Hybrid representation.  Up to `entity-set-small-max' members live in an
  ;; unsorted list (SMALL); beyond that the set is promoted to a growable
  ;; vector of 32-bit words (BITS, SMALL = #f).  Free-variable sets in the
  ;; reify pass are mostly tiny but live in graphs with 10^5 ids, so a dense
  ;; bit vector per set made that pass quadratic.

  (define entity-set-small-max 12)

  (define-record-type (entity-set make-entity-set-record entity-set?)
    (fields (mutable small entity-set-small entity-set-small-set!)
      (mutable bits entity-set-bits entity-set-bits-set!)
      (mutable count entity-set-count entity-set-count-set!)))

  (define (make-entity-set) (make-entity-set-record '() #f 0))

  (define (entity-set-contains? s id)
    (let ([small (entity-set-small s)])
      (if small
        (and (memv id small) #t)
        (let ([bits (entity-set-bits s)] [w (bitwise-arithmetic-shift-right id 5)])
          (and (< w (vector-length bits))
               (not (eqv? 0 (bitwise-and (vector-ref bits w)
                                         (bitwise-arithmetic-shift-left 1 (bitwise-and id 31))))))))))

  (define (entity-set-bit-insert! s id)
    (let* ([bits (entity-set-bits s)] [w (bitwise-arithmetic-shift-right id 5)]
           [bits (if (>= w (vector-length bits))
                   (let ([bigger (make-vector (max (* 2 (vector-length bits)) (+ w 1)) 0)])
                     (let loop ([i 0])
                       (when (< i (vector-length bits))
                         (vector-set! bigger i (vector-ref bits i))
                         (loop (+ i 1))))
                     (entity-set-bits-set! s bigger)
                     bigger)
                   bits)]
           [m (bitwise-arithmetic-shift-left 1 (bitwise-and id 31))]
           [word (vector-ref bits w)])
      (if (eqv? 0 (bitwise-and word m))
        (begin
          (vector-set! bits w (bitwise-ior word m))
          (entity-set-count-set! s (+ 1 (entity-set-count s)))
          #t)
        #f)))

  (define (entity-set-insert! s id)
    (let ([small (entity-set-small s)])
      (if small
        (if (memv id small)
          #f
          (begin
            (entity-set-small-set! s (cons id small))
            (entity-set-count-set! s (+ 1 (entity-set-count s)))
            (when (> (entity-set-count s) entity-set-small-max)
              (entity-set-bits-set! s (make-vector 16 0))
              (entity-set-small-set! s #f)
              (entity-set-count-set! s 0)
              (for-each (lambda (v) (entity-set-bit-insert! s v)) small)
              (entity-set-bit-insert! s id))
            #t))
        (entity-set-bit-insert! s id))))

  (define (entity-set-remove! s id)
    (let ([small (entity-set-small s)])
      (if small
        (if (memv id small)
          (begin
            (entity-set-small-set! s (remv id small))
            (entity-set-count-set! s (- (entity-set-count s) 1))
            #t)
          #f)
        (if (entity-set-contains? s id)
          (let* ([bits (entity-set-bits s)] [w (bitwise-arithmetic-shift-right id 5)])
            (vector-set! bits w
              (bitwise-and (vector-ref bits w)
                           (bitwise-not (bitwise-arithmetic-shift-left 1 (bitwise-and id 31)))))
            (entity-set-count-set! s (- (entity-set-count s) 1))
            #t)
          #f))))

  ;; Adds the members of SRC to DST a word at a time (when SRC is a vector), without
  ;; listing them: listing a vector costs its length, which is the largest id of the
  ;; graph, and the reify pass merges sets more than a million times in a big program.
  (define (entity-set-union! dst src)
    (let ([small (entity-set-small src)])
      (if small
        (for-each (lambda (v) (entity-set-insert! dst v)) small)
        (let* ([sb (entity-set-bits src)] [n (vector-length sb)])
          (let ([members (entity-set-small dst)])
            (when members
              (entity-set-bits-set! dst (make-vector (max 16 n) 0))
              (entity-set-small-set! dst #f)
              (entity-set-count-set! dst 0)
              (for-each (lambda (v) (entity-set-bit-insert! dst v)) members)))
          (when (< (vector-length (entity-set-bits dst)) n)
            (let ([bigger (make-vector n 0)] [old (entity-set-bits dst)])
              (let copy ([i 0])
                (when (< i (vector-length old))
                  (vector-set! bigger i (vector-ref old i))
                  (copy (+ i 1))))
              (entity-set-bits-set! dst bigger)))
          (let ([db (entity-set-bits dst)])
            (let loop ([i 0] [count (entity-set-count dst)])
              (if (= i n)
                (entity-set-count-set! dst count)
                (let ([sw (vector-ref sb i)])
                  (if (eqv? sw 0)
                    (loop (+ i 1) count)
                    (let* ([dw (vector-ref db i)] [nw (bitwise-ior dw sw)])
                      (if (eqv? nw dw)
                        (loop (+ i 1) count)
                        (begin
                          (vector-set! db i nw)
                          (loop (+ i 1)
                            (+ count (- (bitwise-bit-count nw) (bitwise-bit-count dw))))))))))))))))

  ;; A new set with the members of S.
  (define (entity-set-copy s)
    (let ([small (entity-set-small s)])
      (if small
        (make-entity-set-record (list-copy small) #f (entity-set-count s))
        (let* ([bits (entity-set-bits s)] [copy (make-vector (vector-length bits) 0)])
          (let loop ([i 0])
            (when (< i (vector-length bits))
              (vector-set! copy i (vector-ref bits i))
              (loop (+ i 1))))
          (make-entity-set-record #f copy (entity-set-count s))))))

  (define (entity-set-clear! s)
    (entity-set-small-set! s '())
    (entity-set-bits-set! s #f)
    (entity-set-count-set! s 0))

  (define (entity-set->list s)
    (let ([small (entity-set-small s)])
      (if small
        (list-sort < small)
        (let ([bits (entity-set-bits s)])
          (let wloop ([w (- (vector-length bits) 1)] [acc '()])
            (if (< w 0)
              acc
              (let ([word (vector-ref bits w)])
                (if (eqv? 0 word)
                  (wloop (- w 1) acc)
                  (let bloop ([b 31] [acc acc])
                    (cond
                      [(< b 0) (wloop (- w 1) acc)]
                      [(not (eqv? 0 (bitwise-and word (bitwise-arithmetic-shift-left 1 b))))
                       (bloop (- b 1) (cons (+ (* w 32) b) acc))]
                      [else (bloop (- b 1) acc)]))))))))))

  (define (parent-term id) (* 2 id))
  (define (parent-func id) (+ (* 2 id) 1))
  (define (parent-term? p) (even? p))
  (define (parent-func? p) (odd? p))
  (define (parent-id p) (quotient p 2))

  (define-record-type (k-let-val make-k-let-val k-let-val?)
    (fields binder expr body))
  (define-record-type (k-fix make-k-fix k-fix?)
    (fields functions body))
  (define-record-type (k-letk make-k-letk k-letk?)
    (fields functions body))
  (define-record-type (k-if make-k-if k-if?)
    (fields test then else hints))
  (define-record-type (k-branch-prim make-k-branch-prim k-branch-prim?)
    (fields op consequent alternative vars))
  (define-record-type (k-continue make-k-continue k-continue?)
    (fields cont vars))
  (define-record-type (k-app make-k-app k-app?)
    (fields func vars cont))
  (define-record-type (k-raise make-k-raise k-raise?)
    (fields kind vars))

  (define-record-match-pattern (~k-let-val binder expr body) k-let-val?
    (binder k-let-val-binder) (expr k-let-val-expr) (body k-let-val-body))
  (define-record-match-pattern (~k-fix functions body) k-fix?
    (functions k-fix-functions) (body k-fix-body))
  (define-record-match-pattern (~k-letk functions body) k-letk?
    (functions k-letk-functions) (body k-letk-body))
  (define-record-match-pattern (~k-if test then else hints) k-if?
    (test k-if-test) (then k-if-then) (else k-if-else) (hints k-if-hints))
  (define-record-match-pattern (~k-branch-prim op consequent alternative vars) k-branch-prim?
    (op k-branch-prim-op) (consequent k-branch-prim-consequent)
    (alternative k-branch-prim-alternative) (vars k-branch-prim-vars))
  (define-record-match-pattern (~k-continue cont vars) k-continue?
    (cont k-continue-cont) (vars k-continue-vars))
  (define-record-match-pattern (~k-app func vars cont) k-app?
    (func k-app-func) (vars k-app-vars) (cont k-app-cont))
  (define-record-match-pattern (~k-raise kind vars) k-raise?
    (kind k-raise-kind) (vars k-raise-vars))

  ;; The fixed part of a branch-prim: PRIM is the primitive name symbol,
  ;; NARGS operands, NTRUE arguments for the true continuation (the false
  ;; continuation gets the rest), HINTS a two element list.
  (define-record-type (branch-op make-branch-op branch-op?)
    (fields prim nargs ntrue hints))

  (define-record-type (k-literal make-k-literal k-literal?)
    (fields value))
  (define-record-type (k-prim-call make-k-prim-call k-prim-call?)
    (fields prim vars))

  (define-record-match-pattern (~k-literal value) k-literal?
    (value k-literal-value))
  (define-record-match-pattern (~k-prim-call prim vars) k-prim-call?
    (prim k-prim-call-prim) (vars k-prim-call-vars))

  (define-record-type (cps-term make-cps-term cps-term?)
    (fields
      (mutable uplink term-uplink term-uplink-set!)
      (mutable kind term-kind term-kind-set!)
      (mutable source term-source term-source-set!)))

  (define (term-copy t)
    (make-cps-term (term-uplink t) (term-kind t) (term-source t)))

  (define-record-type (cps-expr make-cps-expr cps-expr?)
    (fields
      (mutable uplink expr-uplink expr-uplink-set!)
      (mutable kind expr-kind expr-kind-set!)
      (mutable source expr-source expr-source-set!)))

  (define (expr-copy e)
    (make-cps-expr (expr-uplink e) (expr-kind e) (expr-source e)))

  ;; Fields: name source var vars variadic cont is-variadic body is-rec is-cold
  ;; is-noinline is-reified meta case-group.  VARS is a list of bound vars,
  ;; VARIADIC / CONT bound var or #f, CASE-GROUP fixnum or #f.
  (define-record-type (function make-function function?)
    (fields
      (mutable name function-name function-name-set!)
      (mutable source function-source function-source-set!)
      (mutable var function-var function-var-set!)
      (mutable vars function-vars function-vars-set!)
      (mutable variadic function-variadic function-variadic-set!)
      (mutable cont function-cont function-cont-set!)
      (mutable is-variadic function-is-variadic function-is-variadic-set!)
      (mutable body function-body function-body-set!)
      (mutable is-rec function-is-rec function-is-rec-set!)
      (mutable is-cold function-is-cold function-is-cold-set!)
      (mutable is-noinline function-is-noinline function-is-noinline-set!)
      (mutable is-reified function-is-reified function-is-reified-set!)
      (mutable meta function-meta function-meta-set!)
      (mutable case-group function-case-group function-case-group-set!)))

  (define (function-copy f)
    (make-function
      (function-name f) (function-source f) (function-var f) (function-vars f)
      (function-variadic f) (function-cont f) (function-is-variadic f)
      (function-body f) (function-is-rec f) (function-is-cold f)
      (function-is-noinline f) (function-is-reified f) (function-meta f)
      (function-case-group f)))

  (define-record-type (bound-var-data make-bound-var-data bound-var-data?)
    (fields var (mutable occ bound-var-data-occ bound-var-data-occ-set!)))

  (define-record-type (free-occ-payload make-free-occ-payload free-occ-payload?)
    (fields
      (mutable point free-occ-payload-point free-occ-payload-point-set!)
      (mutable owner free-occ-payload-owner free-occ-payload-owner-set!)
      (mutable prev free-occ-payload-prev free-occ-payload-prev-set!)
      (mutable next free-occ-payload-next free-occ-payload-next-set!)))

  (define-record-type (uf-node make-uf-node uf-node?)
    (fields
      (mutable parent uf-node-parent uf-node-parent-set!)
      (mutable descriptor uf-node-descriptor uf-node-descriptor-set!)))

  (define-record-type (graph make-graph-record graph?)
    (fields
      term-links* expr-links* parent-links* function-links* free-occ-links*
      points* bound-vars* terms* exprs* functions*))

  (define (make-graph)
    (make-graph-record
      (new-arena) (new-arena) (new-arena) (new-arena) (new-arena)
      (new-arena) (new-arena) (new-arena) (new-arena) (new-arena)))

  ;; The raw link arenas (PrimaryMap accessors); see arena-ref / arena->list.
  (define (term-links g) (graph-term-links* g))
  (define (expr-links g) (graph-expr-links* g))
  (define (parent-links g) (graph-parent-links* g))
  (define (function-links g) (graph-function-links* g))

  (define (graph-stats g)
    `((term-links . ,(arena-length (graph-term-links* g)))
      (expr-links . ,(arena-length (graph-expr-links* g)))
      (parent-links . ,(arena-length (graph-parent-links* g)))
      (function-links . ,(arena-length (graph-function-links* g)))
      (free-occurrences . ,(arena-length (graph-free-occ-links* g)))
      (points . ,(arena-length (graph-points* g)))
      (bound-vars . ,(arena-length (graph-bound-vars* g)))
      (terms . ,(arena-length (graph-terms* g)))
      (exprs . ,(arena-length (graph-exprs* g)))
      (functions . ,(arena-length (graph-functions* g)))))

  ;; Index<..> / IndexMut<..>
  (define (graph-term g id) (arena-ref (graph-terms* g) id))
  (define (graph-expr g id) (arena-ref (graph-exprs* g) id))
  (define (graph-function g id) (arena-ref (graph-functions* g) id))
  (define (graph-bound-var g id) (arena-ref (graph-bound-vars* g) id))
  (define (graph-free-occ g id) (arena-ref (graph-free-occ-links* g) id))
  (define (graph-point g id) (arena-ref (graph-points* g) id))
  (define (graph-term-set! g id t) (arena-set! (graph-terms* g) id t))
  (define (graph-expr-set! g id e) (arena-set! (graph-exprs* g) id e))
  (define (graph-function-set! g id f) (arena-set! (graph-functions* g) id f))

  ;; Iterate over all expression nodes: list of expr ids in creation order.
  (define (exprs-iter g)
    (let loop ([i (- (arena-length (graph-exprs* g)) 1)] [acc '()])
      (if (< i 0) acc (loop (- i 1) (cons i acc)))))

  (define (new-term-link g target) (arena-push! (graph-term-links* g) target))
  (define (read-term-link g link) (arena-ref (graph-term-links* g) link))
  (define (set-term-link! g link target) (arena-set! (graph-term-links* g) link target))
  (define (clear-term-link! g link) (arena-set! (graph-term-links* g) link #f))

  (define (new-expr-link g target) (arena-push! (graph-expr-links* g) target))
  (define (read-expr-link g link) (arena-ref (graph-expr-links* g) link))
  (define (set-expr-link! g link target) (arena-set! (graph-expr-links* g) link target))
  (define (clear-expr-link! g link) (arena-set! (graph-expr-links* g) link #f))

  (define (new-parent-link g target) (arena-push! (graph-parent-links* g) target))
  (define (read-parent-link g link) (arena-ref (graph-parent-links* g) link))
  (define (set-parent-link! g link target) (arena-set! (graph-parent-links* g) link target))
  (define (clear-parent-link! g link) (arena-set! (graph-parent-links* g) link #f))

  (define (new-function-link g target) (arena-push! (graph-function-links* g) target))
  (define (read-function-link g link) (arena-ref (graph-function-links* g) link))
  (define (set-function-link! g link target) (arena-set! (graph-function-links* g) link target))
  (define (clear-function-link! g link) (arena-set! (graph-function-links* g) link #f))

  (define (new-bound-var g var)
    (arena-push! (graph-bound-vars* g) (make-bound-var-data var #f)))

  (define (set-bound-var-occ! g var occ)
    (bound-var-data-occ-set! (graph-bound-var g var) occ))

  (define (bound-var-occ g var) (bound-var-data-occ (graph-bound-var g var)))

  (define (binder-is-dead? g var) (not (bound-var-occ g var)))

  (define (new-point g descriptor)
    (let ([point (arena-next-key (graph-points* g))])
      (arena-push! (graph-points* g) (make-uf-node point descriptor))
      point))

  ;; The representative point, compressing the path.
  (define (find-root g point)
    (let ([root (find-root-const g point)])
      (let loop ([cursor point])
        (unless (= cursor root)
          (let ([parent (uf-node-parent (graph-point g cursor))])
            (uf-node-parent-set! (graph-point g cursor) root)
            (loop parent))))
      root))

  (define (find-root-const g point)
    (let loop ([cursor point])
      (let ([parent (uf-node-parent (graph-point g cursor))])
        (if (= parent cursor) cursor (loop parent)))))

  (define (point-descriptor-const g point)
    (uf-node-descriptor (graph-point g (find-root-const g point))))

  (define (free-binder-rooted g occ)
    (point-descriptor-const g
      (find-root-const g (free-occ-payload-point (graph-free-occ g occ)))))

  (define (function-binder-rooted g var)
    (let ([occ (bound-var-occ g var)])
      (if occ (free-binder-rooted g occ) var)))

  ;; Merge POINT's set into REPRESENTATIVE's set (directional).
  (define (union-to! g point representative)
    (let ([mine (find-root g point)]
          [representative (find-root g representative)])
      (unless (= mine representative)
        (uf-node-parent-set! (graph-point g mine) representative))))

  (define (point-descriptor g point)
    (uf-node-descriptor (graph-point g (find-root g point))))

  (define (set-point-descriptor! g point descriptor)
    (uf-node-descriptor-set! (graph-point g (find-root g point)) descriptor))

  (define (points-equal? g left right)
    (= (find-root g left) (find-root g right)))

  (define (free-binder g occ)
    (point-descriptor-const g (free-occ-payload-point (graph-free-occ g occ))))

  (define (free-owner g occ) (free-occ-payload-owner (graph-free-occ g occ)))

  (define (set-free-owner! g occ owner)
    (free-occ-payload-owner-set! (graph-free-occ g occ) owner))

  (define (new-free-occ g point owner)
    (let ([occ (arena-next-key (graph-free-occ-links* g))])
      (arena-push! (graph-free-occ-links* g)
        (make-free-occ-payload point owner occ occ))
      occ))

  (define (free-occ-singleton g point owner) (new-free-occ g point owner))

  (define (free-occ-next g occ) (free-occ-payload-next (graph-free-occ g occ)))
  (define (free-occ-prev g occ) (free-occ-payload-prev (graph-free-occ g occ)))
  (define (set-free-occ-next! g occ next)
    (free-occ-payload-next-set! (graph-free-occ g occ) next))
  (define (set-free-occ-prev! g occ prev)
    (free-occ-payload-prev-set! (graph-free-occ g occ) prev))

  (define (free-occ-is-singleton? g occ)
    (and (= (free-occ-next g occ) occ) (= (free-occ-prev g occ) occ)))

  (define (free-occ-append! g occ point owner)
    (let* ([next (free-occ-next g occ)]
           [inserted (new-free-occ g point owner)])
      (set-free-occ-prev! g inserted occ)
      (set-free-occ-next! g inserted next)
      (set-free-occ-next! g occ inserted)
      (set-free-occ-prev! g next inserted)
      inserted))

  (define (free-occ-merge! g left right)
    (let ([left-prev (free-occ-prev g left)]
          [right-prev (free-occ-prev g right)])
      (set-free-occ-next! g left-prev right)
      (set-free-occ-next! g right-prev left)
      (set-free-occ-prev! g left right-prev)
      (set-free-occ-prev! g right left-prev)))

  (define (free-occ-remove! g occ)
    (let ([prev (free-occ-prev g occ)]
          [next (free-occ-next g occ)])
      (set-free-occ-next! g prev next)
      (set-free-occ-prev! g next prev)
      (set-free-occ-prev! g occ occ)
      (set-free-occ-next! g occ occ)))

  (define (remove-free-occ! g occ)
    (let ([binder (free-binder g occ)]
          [next (free-occ-next g occ)])
      (free-occ-remove! g occ)
      (when (eqv? (bound-var-occ g binder) occ)
        (set-bound-var-occ! g binder (if (= next occ) #f next)))))

  (define (free-occ->list g occ)
    (let loop ([cursor (free-occ-next g occ)] [acc (list occ)])
      (if (= cursor occ)
        (reverse acc)
        (loop (free-occ-next g cursor) (cons cursor acc)))))

  ;; Call F on every occurrence of VAR, starting from its head occurrence.
  (define (for-each-occurrence g var f)
    (let ([occ (bound-var-occ g var)])
      (when occ
        (f occ)
        (let loop ([cursor (free-occ-next g occ)])
          (unless (= cursor occ)
            (f cursor)
            (loop (free-occ-next g cursor)))))))

  (define (collect-occurrences g var)
    (let ([acc '()])
      (for-each-occurrence g var (lambda (occ) (set! acc (cons occ acc))))
      (reverse acc)))

  (define (free-occ-is-singleton-verified? g occ)
    (let ([binder (free-binder g occ)])
      (and (eqv? (bound-var-occ g binder) occ) (free-occ-is-singleton? g occ))))

  (define (binder-is-singleton-or-dead? g var)
    (let ([occ (bound-var-occ g var)])
      (if occ (free-occ-is-singleton? g occ) #t)))

  (define (new-free-occ-for-binder g binder owner)
    (let* ([point (new-point g binder)]
           [existing (bound-var-occ g binder)])
      (if (not existing)
        (let ([occ (free-occ-singleton g point owner)])
          (set-bound-var-occ! g binder occ)
          occ)
        (begin
          (union-to! g point (free-occ-payload-point (graph-free-occ g existing)))
          (free-occ-append! g existing point owner)))))

  (define (merge-free-lists! g replacement removed)
    (let ([replacement-occ (bound-var-occ g replacement)]
          [removed-occ (bound-var-occ g removed)])
      (cond
        [(and (not replacement-occ) (not removed-occ)) (if #f #f)]
        [(not replacement-occ) (set-bound-var-occ! g replacement removed-occ)]
        [(not removed-occ) (set-bound-var-occ! g removed replacement-occ)]
        [else (free-occ-merge! g replacement-occ removed-occ)])))

  (define (subst-binders! g replacement removed)
    (merge-free-lists! g replacement removed)
    (set-bound-var-occ! g removed #f))

  (define (subst-var-for-bound! g replacement removed)
    (let ([removed-occ (bound-var-occ g removed)])
      (when removed-occ
        (subst-var-for-var! g replacement removed-occ))))

  (define (subst-var-for-var! g replacement removed)
    (subst-var-for-var-points! g
      (free-occ-payload-point (graph-free-occ g replacement))
      (free-occ-payload-point (graph-free-occ g removed))))

  (define (subst-var-for-var-points! g replacement removed)
    (unless (points-equal? g replacement removed)
      (let ([replacement-binder (point-descriptor g replacement)]
            [removed-binder (point-descriptor g removed)])
        (subst-binders! g replacement-binder removed-binder)
        (union-to! g removed replacement))))

  (define (subst-var-for-binders! g replacement removed)
    (let ([replacement-occ (bound-var-occ g replacement)]
          [removed-occ (bound-var-occ g removed)])
      (cond
        [(and replacement-occ removed-occ)
          (subst-var-for-var! g replacement-occ removed-occ)]
        [replacement-occ (subst-binders! g replacement removed)]
        [removed-occ
          (let ([removed-point (free-occ-payload-point (graph-free-occ g removed-occ))])
            (subst-binders! g replacement removed)
            (set-point-descriptor! g removed-point replacement))]
        [else (if #f #f)])))

  (define (new-free-vars g vars) vars)
  (define (free-vars-slice g vars) vars)
  (define (free-vars-push g vars var) (append vars (list var)))
  (define (clear-free-vars g vars) '())
  (define (new-bound-vars g vars) vars)
  (define (bound-vars-slice g vars) vars)
  (define (bound-vars-push g vars var) (append vars (list var)))
  (define (new-function-links g links) links)
  (define (function-links-slice g links) links)
  (define (function-links-push g links link) (append links (list link)))

  ;; The operands of a branch-prim as three values: the primitive's
  ;; arguments, the true continuation's arguments, the false one's.
  (define (branch-operands g op vars)
    (let loop ([rest vars] [n (branch-op-nargs op)] [args '()])
      (if (> n 0)
        (loop (cdr rest) (- n 1) (cons (car rest) args))
        (let loop2 ([rest rest] [n (branch-op-ntrue op)] [true-args '()])
          (if (> n 0)
            (loop2 (cdr rest) (- n 1) (cons (car rest) true-args))
            (values (reverse args) (reverse true-args) rest))))))

  ;; Functions in LINKS that are still live, in list order.
  (define (live-functions g links)
    (let loop ([links links] [acc '()])
      (if (null? links)
        (reverse acc)
        (let ([f (read-function-link g (car links))])
          (loop (cdr links) (if f (cons f acc) acc))))))

  ;; The (cont . args) of LINK if it is a direct Continue, else #f.
  (define (direct-continue g link)
    (let ([term (read-term-link g link)])
      (and term
        (let ([kind (term-kind (graph-term g term))])
          (and (k-continue? kind)
            (cons (k-continue-cont kind) (k-continue-vars kind)))))))

  (define (for-each-free-var-of-expr g expr f)
    (let ([kind (expr-kind (graph-expr g expr))])
      (when (k-prim-call? kind)
        (for-each f (k-prim-call-vars kind)))))

  (define (free-vars-of-expr g expr)
    (let ([kind (expr-kind (graph-expr g expr))])
      (if (k-prim-call? kind) (k-prim-call-vars kind) '())))

  ;; Rust's push_free_vars_of_expr appends to a Vec; here ACC is a list and
  ;; the extended list is returned.
  (define (push-free-vars-of-expr g expr acc)
    (append acc (free-vars-of-expr g expr)))

  ;; Whether any free variable of EXPR is bound by a binder in the entity-set
  ;; BINDERS.
  (define (expr-free-vars-contain-any? g expr binders)
    (let ([contains #f])
      (for-each-free-var-of-expr g expr
        (lambda (var)
          (when (entity-set-contains? binders (free-binder g var))
            (set! contains #t))))
      contains))

  (define (direct-free-vars-of-term g term)
    (let ([kind (term-kind (graph-term g term))])
      (cond
        [(or (k-let-val? kind) (k-fix? kind) (k-letk? kind)) '()]
        [(k-if? kind) (list (k-if-test kind))]
        [(k-continue? kind) (cons (k-continue-cont kind) (k-continue-vars kind))]
        [(k-app? kind)
          (cons (k-app-func kind) (append (k-app-vars kind) (list (k-app-cont kind))))]
        [(k-raise? kind) (k-raise-vars kind)]
        [(k-branch-prim? kind)
          (append (k-branch-prim-vars kind)
            (list (k-branch-prim-consequent kind) (k-branch-prim-alternative kind)))]
        [else (assertion-violation 'direct-free-vars-of-term "bad term kind" kind)])))

  (define (for-each-direct-free-var-of-term g term f)
    (for-each f (direct-free-vars-of-term g term)))

  ;; Append the direct free vars of TERM to the list ACC (in order); returns
  ;; the extended list.
  (define (push-direct-free-vars-of-term g term acc)
    (append acc (direct-free-vars-of-term g term)))

  (define (subexprs-of g term)
    (let ([kind (term-kind (graph-term g term))])
      (if (k-let-val? kind) (list (k-let-val-expr kind)) '())))

  (define (for-each-subexpr g term f) (for-each f (subexprs-of g term)))

  (define (subterms-of g term)
    (let ([kind (term-kind (graph-term g term))])
      (cond
        [(k-let-val? kind) (list (k-let-val-body kind))]
        [(k-fix? kind) (list (k-fix-body kind))]
        [(k-letk? kind) (list (k-letk-body kind))]
        [(k-if? kind) (list (k-if-then kind) (k-if-else kind))]
        [else '()])))

  (define (for-each-subterm g term f) (for-each f (subterms-of g term)))

  (define (term-link-points-to? g link term)
    (eqv? (read-term-link g link) term))

  ;; The term link that currently points at TERM, or #f when TERM is dead.
  (define (get-active-link-for g term)
    (let ([parent (read-parent-link g (term-uplink (graph-term g term)))])
      (cond
        [(not parent) #f]
        [(parent-func? parent)
          (let ([body (function-body (graph-function g (parent-id parent)))])
            (and (term-link-points-to? g body term) body))]
        [else
          (let loop ([links (subterms-of g (parent-id parent))] [active #f])
            (cond
              [(null? links) active]
              [(term-link-points-to? g (car links) term)
                (when active
                  (assertion-violation 'get-active-link-for
                    "multiple active subterm links point at the term" term))
                (loop (cdr links) (car links))]
              [else (loop (cdr links) active)]))])))

  (define (replace-active-subterm! g stale-term replacement)
    (let ([status (get-active-link-for g stale-term)])
      (when status (set-term-link! g status replacement))
      status))

  (define (backpatch-subterms! g parent subterms)
    (for-each
      (lambda (subterm)
        (let ([term (read-term-link g subterm)])
          (when term
            (set-parent-link! g (term-uplink (graph-term g term)) (parent-term parent)))))
      subterms))

  (define (backpatch-subexprs! g parent parent-owner subexprs)
    (for-each
      (lambda (subexpr)
        (let ([expr (read-expr-link g subexpr)])
          (when expr
            (for-each (lambda (var) (set-free-owner! g var parent-owner))
              (free-vars-of-expr g expr))
            (set-parent-link! g (expr-uplink (graph-expr g expr)) (parent-term parent)))))
      subexprs))

  (define (backpatch-function! g function)
    (let ([term (read-term-link g (function-body (graph-function g function)))])
      (when term
        (set-parent-link! g (term-uplink (graph-term g term)) (parent-func function)))))

  (define (new-term g uplink kind source)
    (arena-push! (graph-terms* g) (make-cps-term uplink kind source)))

  (define (new-expr g uplink kind source)
    (arena-push! (graph-exprs* g) (make-cps-expr uplink kind source)))

  (define (new-function g function)
    (arena-push! (graph-functions* g) function))

  (define (entity-display-name prefix id)
    (string-append prefix (number->string id)))

  (define (hints->string hints)
    (string-append "[" (symbol->string (car hints)) ", "
      (symbol->string (cadr hints)) "]"))

  (define (fmt-bound-var g p var)
    (if (and (>= var 0) (< var (arena-length (graph-bound-vars* g))))
      (begin
        (display (entity-display-name "bound_var" var) p)
        (display ":" p)
        (display (lvar-name (bound-var-data-var (graph-bound-var g var))) p))
      (begin
        (display (entity-display-name "bound_var" var) p)
        (display ":<missing>" p))))

  (define (fmt-free-var g p var)
    (if (and (>= var 0) (< var (arena-length (graph-free-occ-links* g))))
      (begin
        (display (entity-display-name "free_occ" var) p)
        (display "/" p)
        (fmt-bound-var g p
          (point-descriptor-const g (free-occ-payload-point (graph-free-occ g var)))))
      (begin
        (display (entity-display-name "free_occ" var) p)
        (display "/<missing>" p))))

  (define (fmt-list p items f)
    (display "[" p)
    (let loop ([items items] [first #t])
      (unless (null? items)
        (unless first (display ", " p))
        (f (car items))
        (loop (cdr items) #f)))
    (display "]" p))

  (define (fmt-free-vars g p vars) (fmt-list p vars (lambda (v) (fmt-free-var g p v))))
  (define (fmt-bound-vars g p vars) (fmt-list p vars (lambda (v) (fmt-bound-var g p v))))

  (define (fmt-term-link g p link depth)
    (let ([term (read-term-link g link)])
      (if term
        (fmt-term g p term (+ depth 1))
        (begin (display (entity-display-name "term_link" link) p) (display ":<dead>" p)))))

  (define (fmt-expr-link g p link depth)
    (let ([expr (read-expr-link g link)])
      (if expr
        (fmt-expr g p expr (+ depth 1))
        (begin (display (entity-display-name "expr_link" link) p) (display ":<dead>" p)))))

  (define (fmt-function-link g p link depth)
    (let ([function (read-function-link g link)])
      (if function
        (fmt-function g p function (+ depth 1))
        (begin (display (entity-display-name "function_link" link) p) (display ":<dead>" p)))))

  (define (fmt-function-links g p links depth)
    (fmt-list p links (lambda (l) (fmt-function-link g p l (+ depth 1)))))

  (define (fmt-term g p term depth)
    (if (> depth 32)
      (begin (display (entity-display-name "term" term) p) (display ":..." p))
      (let ([kind (term-kind (graph-term g term))]
            [d (lambda (s) (display s p))])
        (d (entity-display-name "term" term))
        (d " ")
        (cond
          [(k-let-val? kind)
            (d "(let-val ")
            (fmt-bound-var g p (k-let-val-binder kind))
            (d " = ")
            (fmt-expr-link g p (k-let-val-expr kind) (+ depth 1))
            (d " in ")
            (fmt-term-link g p (k-let-val-body kind) (+ depth 1))
            (d ")")]
          [(k-fix? kind)
            (d "(fix ")
            (fmt-function-links g p (k-fix-functions kind) (+ depth 1))
            (d " in ")
            (fmt-term-link g p (k-fix-body kind) (+ depth 1))
            (d ")")]
          [(k-letk? kind)
            (d "(letk ")
            (fmt-function-links g p (k-letk-functions kind) (+ depth 1))
            (d " in ")
            (fmt-term-link g p (k-letk-body kind) (+ depth 1))
            (d ")")]
          [(k-if? kind)
            (d "(if ")
            (fmt-free-var g p (k-if-test kind))
            (d " ")
            (fmt-term-link g p (k-if-then kind) (+ depth 1))
            (d " ")
            (fmt-term-link g p (k-if-else kind) (+ depth 1))
            (d " ")
            (d (hints->string (k-if-hints kind)))
            (d ")")]
          [(k-continue? kind)
            (d "(continue ")
            (fmt-free-var g p (k-continue-cont kind))
            (d " ")
            (fmt-free-vars g p (k-continue-vars kind))
            (d ")")]
          [(k-app? kind)
            (d "(app ")
            (fmt-free-var g p (k-app-func kind))
            (d " ")
            (fmt-free-vars g p (k-app-vars kind))
            (d " ")
            (fmt-free-var g p (k-app-cont kind))
            (d ")")]
          [(k-raise? kind)
            (d "(raise ")
            (d (symbol->string (k-raise-kind kind)))
            (d " ")
            (fmt-free-vars g p (k-raise-vars kind))
            (d ")")]
          [(k-branch-prim? kind)
            (let ([op (k-branch-prim-op kind)])
              (let-values ([(args true-args false-args)
                            (branch-operands g op (k-branch-prim-vars kind))])
                (d "(branch-prim (")
                (d (symbol->string (branch-op-prim op)))
                (for-each (lambda (v) (d " ") (fmt-free-var g p v)) args)
                (d ") (")
                (fmt-free-var g p (k-branch-prim-consequent kind))
                (for-each (lambda (v) (d " ") (fmt-free-var g p v)) true-args)
                (d ") (")
                (fmt-free-var g p (k-branch-prim-alternative kind))
                (for-each (lambda (v) (d " ") (fmt-free-var g p v)) false-args)
                (d ") ")
                (d (hints->string (branch-op-hints op)))
                (d ")")))]))))

  (define (fmt-expr g p expr depth)
    (let ([kind (expr-kind (graph-expr g expr))])
      (display (entity-display-name "expr" expr) p)
      (display " " p)
      (cond
        [(k-literal? kind)
          (display "(literal " p)
          (display (k-literal-value kind) p)
          (display ")" p)]
        [else
          (display "(prim-call " p)
          (display (k-prim-call-prim kind) p)
          (display " " p)
          (fmt-free-vars g p (k-prim-call-vars kind))
          (display ")" p)])))

  (define (fmt-function g p function depth)
    (if (> depth 32)
      (begin (display (entity-display-name "function" function) p) (display ":..." p))
      (let ([data (graph-function g function)])
        (display (entity-display-name "function" function) p)
        (display " (fn " p)
        (fmt-bound-var g p (function-var data))
        (display " " p)
        (fmt-bound-vars g p (function-vars data))
        (when (function-variadic data)
          (display " & " p)
          (fmt-bound-var g p (function-variadic data)))
        (when (function-cont data)
          (display " -> " p)
          (fmt-bound-var g p (function-cont data)))
        (display " body " p)
        (fmt-term-link g p (function-body data) (+ depth 1))
        (display ")" p))))

  ;; display-* return the rendering as a string (Rust returns a Display
  ;; wrapper; pretty-* are the same thing eagerly).
  (define (display-term g term)
    (call-with-string-output-port (lambda (p) (fmt-term g p term 0))))
  (define (display-expr g expr)
    (call-with-string-output-port (lambda (p) (fmt-expr g p expr 0))))
  (define (display-function g function)
    (call-with-string-output-port (lambda (p) (fmt-function g p function 0))))
  (define (pretty-term g term) (display-term g term))
  (define (pretty-expr g expr) (display-expr g expr))
  (define (pretty-function g function) (display-function g function)))

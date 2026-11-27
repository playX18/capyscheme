;; 0-CFA flow analysis over the graph CPS: the abstract value of every bound
;; variable is the set of closure FunctionIds that may flow to it.
;;
;; Port of crates/capy/src/compiler/cps/flow.rs.
;;
;; A Fix member's variable holds its own closure; at App(f, args, cont) every
;; function in flow(f) may be applied and its parameters receive the argument
;; flows; at Continue(c, args) the continuation's parameters receive the
;; argument flows.
;;
;; Outputs: per-function call sites (the App terms where its closure may be
;; applied), closure classes (functions that may reach the same operator
;; position and must agree on representation), and escape.
;;
;; API:
;;   (flow-analysis graph reify) => analysis      Rust FlowAnalysis::new(&graph, &reify)
;;                                                REIFY is a reify-info (core compiler cps reify)
;;   (flow-analysis-function a function) => flow-info or #f
;;   (flow-info-call-sites info)  => list of TermIds (traversal order)
;;   (flow-info-escapes? info)    => boolean
;;   (flow-analysis-class-of a function) => class index or #f
;;   (flow-analysis-classes a)    => list of classes, each a list of FunctionIds
;;                                   (classes ordered by first member in reify order)
;;   (flow-analysis-flow a binder) => ascending list of FunctionIds flowing to BINDER
;;
;; The flow sets are ascending lists (Rust BTreeSet); lookup tables are eqv
;; hashtables and are never iterated, so the results are deterministic.
(library (core compiler cps flow)
  (export flow-analysis flow-analysis? flow-analysis-function flow-analysis-class-of
    flow-analysis-classes flow-analysis-flow
    flow-info? flow-info-call-sites flow-info-escapes?)
  (import (core) (capy) (core compiler cps graph) (core compiler cps reify))

  (define-record-type (flow-info make-flow-info flow-info?)
    (fields (mutable call-sites) (mutable escapes)))

  (define (flow-info-escapes? i) (flow-info-escapes i))

  (define-record-type (flow-analysis %make-flow-analysis flow-analysis?)
    (fields flow-table function-table class-table classes))

  (define (flow-analysis-function a f) (hashtable-ref (flow-analysis-function-table a) f #f))
  (define (flow-analysis-class-of a f) (hashtable-ref (flow-analysis-class-table a) f #f))
  (define (flow-analysis-flow a binder) (hashtable-ref (flow-analysis-flow-table a) binder '()))

  ;; --- sorted-list sets (BTreeSet<FunctionId>) -------------------------------

  ;; Union of ascending lists A and B (no duplicates).
  (define (set-union a b)
    (cond
      [(null? a) b]
      [(null? b) a]
      [(< (car a) (car b)) (cons (car a) (set-union (cdr a) b))]
      [(> (car a) (car b)) (cons (car b) (set-union a (cdr b)))]
      [else (cons (car a) (set-union (cdr a) (cdr b)))]))

  (define (flow-analysis g reify)
    (let ([flow (make-eqv-hashtable)]            ; binder -> ascending list
          [callable-by-binder (make-eqv-hashtable)]
          [params (make-eqv-hashtable)]
          [functions (reify-info-functions reify)]
          [continuations (reify-info-continuations reify)])

      (define (flow-ref binder) (hashtable-ref flow binder '()))

      ;; Add SRC to the flow set of PARAM, report whether it grew.
      (define (flow-into! param src)
        (let* ([old (flow-ref param)]
               [new (set-union old src)])
          (if (= (length new) (length old))
            #f
            (begin (hashtable-set! flow param new) #t))))

      (define (flow-args! ps args-binders)
        (let loop ([ps ps] [as args-binders] [changed #f])
          (if (or (null? ps) (null? as))
            changed
            (let ([arg-flow (flow-ref (car as))])
              (loop (cdr ps) (cdr as)
                (if (null? arg-flow)
                  changed
                  (or (flow-into! (car ps) arg-flow) changed)))))))

      (define (rooted-binders vars)
        (map (lambda (v) (free-binder-rooted g v)) vars))

      ;; Let the functions flowing into ARGS flow into the parameters of CONT.
      (define (flow-into-continuation cont args)
        (let* ([cont-binder (free-binder-rooted g cont)]
               [args-binders (rooted-binders args)]
               [continuation (hashtable-ref callable-by-binder cont-binder #f)])
          (and continuation
            (let ([ps (hashtable-ref params continuation #f)])
              (and ps (flow-args! ps args-binders))))))

      ;; Propagate one term's App/Continue constraints.
      (define (propagate-term term)
        (let ([kind (term-kind (graph-term g term))])
          (cond
            [(k-app? kind)
              (let* ([op-binder (free-binder-rooted g (k-app-func kind))]
                     [callees (flow-ref op-binder)]
                     [args-binders (rooted-binders (free-vars-slice g (k-app-vars kind)))])
                (let loop ([callees callees] [changed #f])
                  (if (null? callees)
                    changed
                    (let ([ps (hashtable-ref params (car callees) #f)])
                      (loop (cdr callees)
                        (if ps (or (flow-args! ps args-binders) changed) changed))))))]
            [(k-continue? kind)
              (flow-into-continuation (k-continue-cont kind)
                (free-vars-slice g (k-continue-vars kind)))]
            [(k-branch-prim? kind)
              (let-values ([(args true-args false-args)
                            (branch-operands g (k-branch-prim-op kind) (k-branch-prim-vars kind))])
                (let* ([a (flow-into-continuation (k-branch-prim-consequent kind) true-args)]
                       [b (flow-into-continuation (k-branch-prim-alternative kind) false-args)])
                  (or a b)))]
            [else #f])))

      ;; Visit every term reachable from ROOT (deduplicated).
      (define (for-term root visited f)
        (define (walk-link link)
          (let ([term (read-term-link g link)])
            (when term (visit term))))
        (define (visit term)
          (unless (hashtable-contains? visited term)
            (hashtable-set! visited term #t)
            (f term)
            (let ([kind (term-kind (graph-term g term))])
              (cond
                [(k-let-val? kind) (walk-link (k-let-val-body kind))]
                [(or (k-fix? kind) (k-letk? kind))
                  (for-each
                    (lambda (fn) (walk-link (function-body (graph-function g fn))))
                    (live-functions g (function-links-slice g
                      (if (k-fix? kind) (k-fix-functions kind) (k-letk-functions kind)))))
                  (walk-link (if (k-fix? kind) (k-fix-body kind) (k-letk-body kind)))]
                [(k-if? kind)
                  (walk-link (k-if-then kind))
                  (walk-link (k-if-else kind))]
                [else (if #f #f)]))))
        (visit root))

      ;; Root-resolved binder -> function/continuation map.
      (for-each
        (lambda (f)
          (hashtable-set! callable-by-binder
            (function-binder-rooted g (function-var (graph-function g f))) f))
        functions)
      (for-each
        (lambda (c)
          (hashtable-set! callable-by-binder
            (function-binder-rooted g (function-var (graph-function g c))) c))
        continuations)

      ;; Parameters of each callable, in order.
      (for-each
        (lambda (f)
          (hashtable-set! params f
            (bound-vars-slice g (function-vars (graph-function g f)))))
        (append functions continuations))

      ;; Seed: each function's own var flows its own closure.
      (for-each
        (lambda (f)
          (let ([binder (function-binder-rooted g (function-var (graph-function g f)))])
            (hashtable-set! flow binder (set-union (flow-ref binder) (list f)))))
        functions)

      (let* ([root-link (function-body (graph-function g (reify-info-entrypoint reify)))]
             [root (read-term-link g root-link)])
        (unless root (error 'flow-analysis "graph root term"))

        ;; Fixed-point propagation over all terms.
        (let loop ()
          (let ([changed #f]
                [visited (make-eqv-hashtable)])
            (for-term root visited
              (lambda (term) (when (propagate-term term) (set! changed #t))))
            (when changed (loop))))

        ;; Call web + escape detection.
        (let ([by-function (make-eqv-hashtable)]
              [escaped (make-eqv-hashtable)])
          (define (info-of f)
            (or (hashtable-ref by-function f #f)
              (let ([i (make-flow-info '() #f)])
                (hashtable-set! by-function f i)
                i)))
          (define (escape-var! binder)
            (for-each (lambda (f) (hashtable-set! escaped f #t)) (flow-ref binder)))
          (define (escape-vars! vars)
            (for-each (lambda (v) (escape-var! (free-binder-rooted g v))) vars))
          (define (record-call! op-binder site)
            (for-each
              (lambda (f)
                (let ([i (info-of f)])
                  (flow-info-call-sites-set! i (cons site (flow-info-call-sites i)))))
              (flow-ref op-binder)))

          (define (use-visit term)
            (let ([kind (term-kind (graph-term g term))])
              (cond
                [(k-let-val? kind)
                  (let ([expr (read-expr-link g (k-let-val-expr kind))])
                    (when expr
                      (let ([ek (expr-kind (graph-expr g expr))])
                        (when (k-prim-call? ek)
                          (escape-vars! (free-vars-slice g (k-prim-call-vars ek)))))))]
                [(k-if? kind) (escape-var! (free-binder-rooted g (k-if-test kind)))]
                [(k-app? kind)
                  (let ([op-binder (free-binder-rooted g (k-app-func kind))])
                    (record-call! op-binder term)
                    ;; Arguments passed to an unknown callee escape.
                    (when (null? (flow-ref op-binder))
                      (escape-vars! (free-vars-slice g (k-app-vars kind)))))]
                [(k-raise? kind) (escape-vars! (free-vars-slice g (k-raise-vars kind)))]
                [(k-branch-prim? kind)
                  (let-values ([(args true-args false-args)
                                (branch-operands g (k-branch-prim-op kind) (k-branch-prim-vars kind))])
                    (escape-vars! args))]
                [else (if #f #f)])))

          (for-each (lambda (f) (info-of f)) functions)
          (for-term root (make-eqv-hashtable) use-visit)

          ;; Closure classes: union functions that may apply at the same site.
          (let ([parent (make-eqv-hashtable)])
            (define (uf-find f)
              (let ([p (hashtable-ref parent f f)])
                (if (eqv? p f)
                  f
                  (let ([root (uf-find p)])
                    (hashtable-set! parent f root)
                    root))))
            (define (uf-union! a b)
              (let ([ra (uf-find a)] [rb (uf-find b)])
                (unless (eqv? ra rb) (hashtable-set! parent ra rb))))
            (for-each (lambda (f) (hashtable-set! parent f f)) functions)
            (for-term root (make-eqv-hashtable)
              (lambda (term)
                (let ([kind (term-kind (graph-term g term))])
                  (when (k-app? kind)
                    (let ([ops (flow-ref (free-binder-rooted g (k-app-func kind)))])
                      (when (and (pair? ops) (pair? (cdr ops)))
                        (for-each (lambda (op) (uf-union! (car ops) op)) (cdr ops))))))))

            (let ([class-of (make-eqv-hashtable)]
                  [index (make-eqv-hashtable)]     ; rep -> class index
                  [classes (make-vector (length functions) '())]
                  [count 0])
              (for-each
                (lambda (f)
                  (let ([rep (uf-find f)])
                    (unless (hashtable-contains? index rep)
                      (hashtable-set! index rep count)
                      (set! count (+ count 1)))
                    (let ([i (hashtable-ref index rep #f)])
                      (vector-set! classes i (cons f (vector-ref classes i)))
                      (hashtable-set! class-of f i))))
                functions)
              (for-each
                (lambda (f)
                  (flow-info-escapes-set! (info-of f) (hashtable-contains? escaped f)))
                functions)
              (for-each
                (lambda (f)
                  (let ([i (info-of f)])
                    (flow-info-call-sites-set! i (reverse (flow-info-call-sites i)))))
                (hashtable-keys-list by-function))
              (%make-flow-analysis flow by-function class-of
                (let loop ([i (- count 1)] [acc '()])
                  (if (< i 0)
                    acc
                    (loop (- i 1) (cons (reverse (vector-ref classes i)) acc)))))))))))

  (define (hashtable-keys-list t)
    (vector->list (hashtable-keys t)))
)

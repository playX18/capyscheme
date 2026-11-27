;; Reifying continuations
(library (core compiler cps reify)
  (export reify-graph! make-reify-info reify-info?
    reify-info-entrypoint reify-info-functions reify-info-continuations
    reify-info-free-vars
    reify-free-vars-function reify-free-vars-continuation
    reify-free-vars-function-of-binder reify-free-vars-continuation-of-binder
    reify-free-vars-continuation-value?
    binder-set->list)
  (import (core) (capy) (core compiler cps graph))

  (define-record-type (reify-info make-reify-info reify-info?)
    (fields entrypoint functions continuations free-vars))

  ;; Rust GraphFreeVars.  The maps are eqv hashtables used for lookup only
  ;; (never iterated).
  (define-record-type (reify-free-vars make-reify-free-vars reify-free-vars?)
    (fields function-vars continuation-vars functions-by-binder
      continuations-by-binder continuation-values))

  (define (new-free-vars-record)
    (make-reify-free-vars (make-eqv-hashtable) (make-eqv-hashtable)
      (make-eqv-hashtable) (make-eqv-hashtable) (make-entity-set)))

  (define (reify-free-vars-function fv function)
    (or (hashtable-ref (reify-free-vars-function-vars fv) function #f)
      (make-entity-set)))

  (define (reify-free-vars-continuation fv continuation)
    (or (hashtable-ref (reify-free-vars-continuation-vars fv) continuation #f)
      (make-entity-set)))

  (define (reify-free-vars-function-of-binder fv var)
    (hashtable-ref (reify-free-vars-functions-by-binder fv) var #f))

  (define (reify-free-vars-continuation-of-binder fv var)
    (hashtable-ref (reify-free-vars-continuations-by-binder fv) var #f))

  (define (reify-free-vars-continuation-value? fv var)
    (entity-set-contains? (reify-free-vars-continuation-values fv) var))

  (define (binder-set->list s) (entity-set->list s))

  (define (set-extend! dst src) (entity-set-union! dst src))

  (define (set-copy s) (entity-set-copy s))

  ;; The union of the sets A and B, made by adding the smaller one to the bigger one:
  ;; both are the caller's, and the free variables of a long chain of terms would
  ;; otherwise be copied once for every term of it.
  (define (set-union! a b)
    (if (>= (entity-set-count a) (entity-set-count b))
      (begin (set-extend! a b) a)
      (begin (set-extend! b a) b)))

  (define (set-from-list vars)
    (let ([out (make-entity-set)])
      (for-each (lambda (v) (entity-set-insert! out v)) vars)
      out))

  (define (reify-graph! g entrypoint)
    (let ([free-vars (new-free-vars-record)]
          [functions '()]            ; reversed
          [continuations '()]        ; reversed
          [seen-functions (make-entity-set)]
          [seen-continuations (make-entity-set)]
          [computed-functions (make-entity-set)])

      (define (record-function function)
        (let ([data (graph-function g function)])
          (if (function-cont data)
            (begin
              (when (entity-set-insert! seen-functions function)
                (set! functions (cons function functions)))
              (hashtable-set! (reify-free-vars-functions-by-binder free-vars)
                (function-var data) function))
            (begin
              (when (entity-set-insert! seen-continuations function)
                (set! continuations (cons function continuations)))
              (hashtable-set! (reify-free-vars-continuations-by-binder free-vars)
                (function-var data) function)))))

      (define (vars-for-function function)
        (if (function-cont (graph-function g function))
          (reify-free-vars-function free-vars function)
          (reify-free-vars-continuation free-vars function)))

      (define (collect-function function)
        (record-function function)
        (if (entity-set-contains? computed-functions function)
          (set-copy (vars-for-function function))
          (begin
            (entity-set-insert! computed-functions function)
            (let* ([data (graph-function g function)]
                   [vars (collect-term (function-body data))])
              (for-each (lambda (v) (entity-set-remove! vars v))
                (bound-vars-slice g (function-vars data)))
              (when (function-variadic data)
                (entity-set-remove! vars (function-variadic data)))
              (if (function-cont data)
                (begin
                  (entity-set-remove! vars (function-cont data))
                  (hashtable-set! (reify-free-vars-function-vars free-vars)
                    function (set-copy vars)))
                (begin
                  (entity-set-remove! vars (function-var data))
                  (hashtable-set! (reify-free-vars-continuation-vars free-vars)
                    function (set-copy vars))))
              vars))))

      (define (singleton-free-var var)
        (set-from-list (list (free-binder g var))))

      (define (free-vars-from-occurrences vars)
        (set-from-list (map (lambda (v) (free-binder g v)) (free-vars-slice g vars))))

      (define (collect-expr expr)
        (let ([kind (expr-kind (graph-expr g expr))])
          (if (k-prim-call? kind)
            (let ([vars (free-vars-from-occurrences (k-prim-call-vars kind))])
              (for-each
                (lambda (binder)
                  (when (hashtable-ref (reify-free-vars-continuations-by-binder free-vars) binder #f)
                    (entity-set-insert! (reify-free-vars-continuation-values free-vars) binder)))
                (entity-set->list vars))
              vars)
            (make-entity-set))))

      (define (collect-term link)
        (let ([term (read-term-link g link)])
          (if (not term)
            (make-entity-set)
            (let ([kind (term-kind (graph-term g term))])
              (cond
                [(k-let-val? kind)
                  (let* ([expr (read-expr-link g (k-let-val-expr kind))]
                         [expr-vars (if expr (collect-expr expr) (make-entity-set))]
                         [vars (set-union! expr-vars (collect-term (k-let-val-body kind)))])
                    (entity-set-remove! vars (k-let-val-binder kind))
                    vars)]
                [(or (k-fix? kind) (k-letk? kind))
                  (let* ([links (if (k-fix? kind) (k-fix-functions kind) (k-letk-functions kind))]
                         [body (if (k-fix? kind) (k-fix-body kind) (k-letk-body kind))]
                         [live (live-functions g (function-links-slice g links))]
                         [vars (begin
                                 (for-each record-function live)
                                 (let* ([vars (fold-left (lambda (vars f) (set-union! vars (collect-function f)))
                                                (make-entity-set) live)])
                                   (set-union! vars (collect-term body))))])
                    (for-each
                      (lambda (f) (entity-set-remove! vars (function-var (graph-function g f))))
                      live)
                    vars)]
                [(k-if? kind)
                  (let* ([vars (set-union! (singleton-free-var (k-if-test kind))
                                 (collect-term (k-if-then kind)))])
                    (set-union! vars (collect-term (k-if-else kind))))]
                [(k-continue? kind)
                  (let ([vars (singleton-free-var (k-continue-cont kind))])
                    (set-extend! vars (free-vars-from-occurrences (k-continue-vars kind)))
                    vars)]
                [(k-app? kind)
                  (let ([cont-binder (free-binder g (k-app-cont kind))])
                    (entity-set-insert! (reify-free-vars-continuation-values free-vars) cont-binder)
                    (let ([vars (singleton-free-var (k-app-func kind))])
                      (set-extend! vars (free-vars-from-occurrences (k-app-vars kind)))
                      (entity-set-insert! vars cont-binder)
                      vars))]
                [(k-raise? kind) (free-vars-from-occurrences (k-raise-vars kind))]
                [else                    ; branch-prim
                  (let ([vars (singleton-free-var (k-branch-prim-consequent kind))])
                    (set-extend! vars (singleton-free-var (k-branch-prim-alternative kind)))
                    (set-extend! vars (free-vars-from-occurrences (k-branch-prim-vars kind)))
                    vars)])))))

      (record-function entrypoint)
      (collect-function entrypoint)
      (hashtable-set! (reify-free-vars-function-vars free-vars) entrypoint (make-entity-set))
      ;; Continuations are second-class: none is a closure.
      (for-each
        (lambda (c) (function-is-reified-set! (graph-function g c) #f))
        (reverse continuations))
      (make-reify-info entrypoint (reverse functions) (reverse continuations) free-vars)))
)

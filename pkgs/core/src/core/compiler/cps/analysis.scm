;; Closure-optimization analyses: stage (first/last-use) analysis and
;; recursion detection.
(library (core compiler cps analysis)
  (export
    stage-max function-stages stages-first stages-last stages-reused-before-own?
    make-stage-analysis stage-analysis-function recursive-functions)
  (import (core) (capy) (core compiler cps graph))

  ;; usize::MAX stand-in.
  (define stage-max (- (expt 2 60) 1))

  (define-record-type (stages make-stages stages?)
    (fields uses))                         ; eqv-hashtable: bound var -> (first . last)

  (define (stages-first s var)
    (let ([e (hashtable-ref (stages-uses s) var #f)])
      (if e (car e) stage-max)))

  (define (stages-last s var)
    (let ([e (hashtable-ref (stages-uses s) var #f)])
      (if e (cdr e) 0)))

  ;; Whether every use of every REUSED variable precedes every use of every
  ;; OWN variable.  Vacuously true when either side is empty.
  (define (stages-reused-before-own? s reused own)
    (if (or (null? reused) (null? own))
      #t
      (let ([last-reused (fold-left max 0 (map (lambda (v) (stages-last s v)) reused))]
            [first-own (fold-left min stage-max (map (lambda (v) (stages-first s v)) own))])
        (< last-reused first-own))))

  (define-record-type (stage-analysis %make-stage-analysis stage-analysis?)
    (fields by-function))                  ; eqv-hashtable: function id -> stages

  (define (make-stage-analysis g reify-functions)
    (let ([table (make-eqv-hashtable)])
      (for-each
        (lambda (f) (hashtable-set! table f (function-stages g f)))
        reify-functions)
      (%make-stage-analysis table)))

  (define (stage-analysis-function a function)
    (hashtable-ref (stage-analysis-by-function a) function #f))


  (define (function-stages g function)
    (let ([uses (make-eqv-hashtable)]
          [counter 0])
      (define (record! var position)
        (let ([e (hashtable-ref uses var #f)])
          (if e
            (begin
              (set-car! e (min (car e) position))
              (set-cdr! e (max (cdr e) position)))
            (hashtable-set! uses var (cons position position)))))
      (define (record-all! vars position)
        (for-each (lambda (v) (record! (free-binder g v) position)) vars))
      (define (walk-expr expr)
        (set! counter (+ counter 1))
        (let ([position counter]
              [kind (expr-kind (graph-expr g expr))])
          (when (k-prim-call? kind)
            (record-all! (k-prim-call-vars kind) position))))
      (define (walk link)
        (let ([term (read-term-link g link)])
          (when term
            (set! counter (+ counter 1))
            (let ([position counter]
                  [kind (term-kind (graph-term g term))])
              (cond
                [(k-let-val? kind)
                  (let ([expr (read-expr-link g (k-let-val-expr kind))])
                    (when expr (walk-expr expr)))
                  (walk (k-let-val-body kind))]
                [(or (k-fix? kind) (k-letk? kind))
                  (let ([functions (if (k-fix? kind) (k-fix-functions kind) (k-letk-functions kind))]
                        [body (if (k-fix? kind) (k-fix-body kind) (k-letk-body kind))])
                    (for-each
                      (lambda (f) (walk (function-body (graph-function g f))))
                      (live-functions g functions))
                    (walk body))]
                [(k-if? kind)
                  (record! (free-binder g (k-if-test kind)) position)
                  (walk (k-if-then kind))
                  (walk (k-if-else kind))]
                [(k-continue? kind) (record-all! (k-continue-vars kind) position)]
                [(k-branch-prim? kind) (record-all! (k-branch-prim-vars kind) position)]
                [(k-app? kind)
                  (record! (free-binder g (k-app-func kind)) position)
                  (record-all! (k-app-vars kind) position)
                  (record! (free-binder g (k-app-cont kind)) position)]
                [(k-raise? kind) (record-all! (k-raise-vars kind) position)])))))
      (walk (function-body (graph-function g function)))
      (make-stages uses)))

  ;; Edge f -> g when f's body calls a variable bound to g.  A function is
  ;; recursive when it lies on a cycle of that direct call graph.

  (define (recursive-functions g reify-functions)
    (let ([functions-by-binder (make-eqv-hashtable)]
          [callees (make-eqv-hashtable)]
          [recursive (make-entity-set)])
      ;; DFS cycle detection; state: 1 = in stack, 2 = done.
      (define (reaches-cycle? function state)
        (let ([s (hashtable-ref state function 0)])
          (cond
            [(= s 1) #t]
            [(= s 2) #f]
            [else
              (hashtable-set! state function 1)
              (let loop ([targets (hashtable-ref callees function '())])
                (cond
                  [(null? targets)
                    (hashtable-set! state function 2)
                    #f]
                  [(reaches-cycle? (car targets) state) #t]
                  [else (loop (cdr targets))]))])))
      ;; Root-resolved binder -> function (the optimizer merges binders via
      ;; the union-find, the raw var may be stale).
      (for-each
        (lambda (f)
          (hashtable-set! functions-by-binder
            (function-binder-rooted g (function-var (graph-function g f)))
            f))
        reify-functions)
      (for-each
        (lambda (f)
          (let ([acc '()])
            (define (walk link)
              (let ([term (read-term-link g link)])
                (when term
                  (let ([kind (term-kind (graph-term g term))])
                    (cond
                      [(k-let-val? kind) (walk (k-let-val-body kind))]
                      [(or (k-fix? kind) (k-letk? kind))
                        (let ([functions (if (k-fix? kind) (k-fix-functions kind) (k-letk-functions kind))]
                              [body (if (k-fix? kind) (k-fix-body kind) (k-letk-body kind))])
                          (for-each
                            (lambda (m) (walk (function-body (graph-function g m))))
                            (live-functions g functions))
                          (walk body))]
                      [(k-if? kind)
                        (walk (k-if-then kind))
                        (walk (k-if-else kind))]
                      [(k-app? kind)
                        (let* ([binder (free-binder-rooted g (k-app-func kind))]
                               [target (hashtable-ref functions-by-binder binder #f)])
                          (when target (set! acc (cons target acc))))]
                      [else (if #f #f)])))))
            (walk (function-body (graph-function g f)))
            (hashtable-set! callees f (reverse acc))))
        reify-functions)
      (for-each
        (lambda (f)
          (when (reaches-cycle? f (make-eqv-hashtable))
            (entity-set-insert! recursive f)))
        reify-functions)
      recursive)))

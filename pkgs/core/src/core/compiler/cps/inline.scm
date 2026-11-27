;; Inlining for graph CPS.
;;
;; Port of crates/capy/src/compiler/cps/inline.rs.
;;
;; API:
;;   (inline-mode-from-flag string) => 'Off 'Leaf 'Known 'Both
;;   (inline-graph graph entry mode max-size small product) => inline-stats
;;       `ctx' (MutationContext) is dropped from the Rust signature.
;;   inline-stats: (make-inline-stats calls functions dead) inline-stats?
;;       inline-stats-calls-inlined inline-stats-functions-inlined
;;       inline-stats-dead-functions, (inline-stats-add a b) => a + b
;;       (zero-inline-stats)
;;
;; Determinism: Rust keeps FunctionInfo in a HashMap<BoundVar, _> and iterates
;; it for scanning, call-site distribution, the self-call set, the eligibility
;; test and the call-graph build.  None of those results depends on iteration
;; order (they fill per-binder fields or sets; `eligible' is sorted by binder
;; afterwards, and only components of size > 1 matter in the SCC step).  Here
;; the map is an eqv hashtable (lookup only) plus an insertion-ordered binder
;; list that is iterated instead.
(library (core compiler cps inline)
  (export
    inline-mode-from-flag inline-graph
    make-inline-stats inline-stats? inline-stats-calls-inlined
    inline-stats-functions-inlined inline-stats-dead-functions
    inline-stats-add zero-inline-stats)
  (import (core) (capy) (core compiler cps graph) (core compiler cps clone)
    (core compiler cps scc-contify))

  (define-record-type (inline-stats make-inline-stats inline-stats?)
    (fields calls-inlined functions-inlined dead-functions))

  (define (zero-inline-stats) (make-inline-stats 0 0 0))

  (define (inline-stats-add a b)
    (make-inline-stats
      (+ (inline-stats-calls-inlined a) (inline-stats-calls-inlined b))
      (+ (inline-stats-functions-inlined a) (inline-stats-functions-inlined b))
      (+ (inline-stats-dead-functions a) (inline-stats-dead-functions b))))

  (define (inline-mode-from-flag value)
    (cond
      [(string=? value "leaf") 'Leaf]
      [(member value '("known" "nonrecursive")) 'Known]
      [(member value '("both" "all" "1" "on" "true")) 'Both]
      [else 'Off]))

  ;; CallSite: (link . caller) with caller a function id or #f.
  (define (make-call-site link caller) (cons link caller))
  (define (call-site-link s) (car s))
  (define (call-site-caller s) (cdr s))

  (define-record-type (function-info make-function-info function-info?)
    (fields
      id binder def-link body
      (mutable size function-info-size info-size-set!)
      (mutable calls function-info-calls info-calls-set!)
      (mutable has-loop function-info-has-loop info-has-loop-set!)
      all-uses-are-calls
      (mutable call-sites function-info-call-sites info-call-sites-set!)))

  (define-record-type (body-scan make-body-scan body-scan?)
    (fields
      (mutable terms scan-terms scan-terms-set!)
      (mutable exprs scan-exprs scan-exprs-set!)
      (mutable calls scan-calls scan-calls-set!)
      (mutable has-loop scan-has-loop scan-has-loop-set!)))

  (define (new-scan) (make-body-scan 0 0 0 #f))

  (define (saturating-sub a b) (max 0 (- a b)))

  (define (allow-by-mode? mode leaf known)
    (case mode
      [(Leaf) leaf]
      [(Known) known]
      [(Both) (or leaf known)]
      [else #f]))

  ;; Inline eligible calls in GRAPH.
  (define (inline-graph g entry mode max-size small product)
    (if (eq? mode 'Off)
      (zero-inline-stats)
      (let ([functions (collect-functions g (function-body (graph-function g entry)))]
            [infos (make-eqv-hashtable)]
            [binder-order '()])
        (for-each
          (lambda (entry-pair)
            (let* ([id (car entry-pair)]
                   [def-link (cdr entry-pair)]
                   [data (graph-function g id)])
              (unless (or (not (function-cont data))
                          (eqv? id entry)
                          (function-is-variadic data)
                          (function-variadic data)
                          (function-case-group data)
                          (function-is-noinline data)
                          (function-is-cold data))
                (let ([binder (function-binder-rooted g (function-var data))])
                  (unless (hashtable-ref infos binder #f)
                    (set! binder-order (cons binder binder-order)))
                  (hashtable-set! infos binder
                    (make-function-info id binder def-link (function-body data)
                      0 0 #f (all-uses-are-calls? g binder) '()))))))
          functions)
        (set! binder-order (reverse binder-order))
        (for-each
          (lambda (binder)
            (let ([info (hashtable-ref infos binder #f)]
                  [scan (new-scan)])
              (scan-body! g (function-info-body info) scan)
              (info-size-set! info (+ (scan-terms scan) (scan-exprs scan)))
              (info-calls-set! info (scan-calls scan))
              (info-has-loop-set! info (scan-has-loop scan))))
          binder-order)
        ;; All call sites in the program, keyed by callee binder.
        (let ([sites-by-callee (make-eqv-hashtable)])
          (collect-call-sites! g (function-body (graph-function g entry)) #f sites-by-callee)
          (for-each
            (lambda (binder)
              (info-call-sites-set! (hashtable-ref infos binder #f)
                (reverse (hashtable-ref sites-by-callee binder '()))))
            binder-order))
        ;; Non-recursive eligibility from the call graph.
        (let ([self-calls (make-entity-set)])
          (for-each
            (lambda (binder)
              (let ([info (hashtable-ref infos binder #f)])
                (when (exists (lambda (site) (eqv? (call-site-caller site) (function-info-id info)))
                        (function-info-call-sites info))
                  (entity-set-insert! self-calls binder))))
            binder-order)
          (let ([mutually-recursive (recursive-binders g infos binder-order)]
                [eligible '()])
            (for-each
              (lambda (binder)
                (let ([info (hashtable-ref infos binder #f)])
                  (unless (or (null? (function-info-call-sites info))
                              (entity-set-contains? self-calls binder)
                              (entity-set-contains? mutually-recursive binder))
                    (let* ([leaf (and (= (function-info-calls info) 0)
                                      (not (function-info-has-loop info))
                                      (<= (function-info-size info) max-size))]
                           [known (and (not (entity-set-contains? self-calls binder))
                                       (<= (* (copies-kept info)
                                              (saturating-sub (function-info-size info) small))
                                           product))])
                      (when (allow-by-mode? mode leaf known)
                        (set! eligible (cons binder eligible)))))))
              binder-order)
            (let ([eligible (list-sort < eligible)]
                  [calls-inlined 0] [functions-inlined 0] [dead-functions 0])
              (for-each
                (lambda (binder)
                  (let* ([info (hashtable-ref infos binder #f)]
                         [callee (function-info-id info)]
                         [sites (function-info-call-sites info)]
                         [now (new-scan)])
                    (scan-body! g (function-body (graph-function g callee)) now)
                    (let* ([size (+ (scan-terms now) (scan-exprs now))]
                           [leaf (and (= (scan-calls now) 0)
                                      (not (scan-has-loop now))
                                      (<= size max-size))]
                           [known (<= (* (copies-kept info)
                                         (saturating-sub size small))
                                      product)])
                      (when (allow-by-mode? mode leaf known)
                        (let ([inlined-here #f])
                          (for-each
                            (lambda (site)
                              (when (inline-site g callee (call-site-link site))
                                (set! calls-inlined (+ calls-inlined 1))
                                (set! inlined-here #t)))
                            sites)
                          (when inlined-here
                            (set! functions-inlined (+ functions-inlined 1)))
                          ;; All uses were calls, so an empty ring means the closure is dead.
                          (when (null? (collect-occurrences g binder))
                            (clear-function-link! g (function-info-def-link info))
                            (set! dead-functions (+ dead-functions 1))))))))
                eligible)
              (make-inline-stats calls-inlined functions-inlined dead-functions)))))))

;; How many copies of the body of the function of INFO exist once every call site is
  ;; inlined, beyond the one that was there before. A function whose every use is a call
  ;; dies, so its last site is free. One that escapes too (an exported definition, stored
  ;; into its variable) keeps its closure, so every site adds a copy. Cloning never changes
  ;; the original, so inlining an escaping function is sound: the escaped closure and the
  ;; inlined copies run the same code.
  (define (copies-kept info)
    (let ([sites (length (function-info-call-sites info))])
      (if (function-info-all-uses-are-calls info)
        (saturating-sub sites 1)
        sites)))

  (define (inline-site g callee site)
    (let ([term (read-term-link g site)])
      (and term
        (let ([kind (term-kind (graph-term g term))])
          (and (k-app? kind)
            (let ([callee-occ (k-app-func kind)]
                  [args (k-app-vars kind)]
                  [cont (k-app-cont kind)]
                  [data (graph-function g callee)])
              (and (eqv? (free-binder-rooted g callee-occ)
                     (function-binder-rooted g (function-var data)))
                (let ([formals (bound-vars-slice g (function-vars data))]
                      [actuals (free-vars-slice g args)]
                      [body (function-body data)]
                      [return-cont (function-cont data)])
                  (and (= (length formals) (length actuals))
                    return-cont
                    (let* ([substitutions
                            (append
                              (map (lambda (formal actual)
                                    (cons formal (free-binder-rooted g actual)))
                                formals actuals)
                              (list (cons return-cont (free-binder-rooted g cont))))]
                           [old-occurrences (push-direct-free-vars-of-term g term '())]
                           [parent (read-parent-link g (term-uplink (graph-term g term)))]
                           [cloner (make-graph-clone/substitutions g substitutions)]
                           [cloned (graph-clone-subterm-into! cloner body site)])
                      (and cloned
                        (begin
                          (for-each (lambda (occ) (remove-free-occ! g occ)) old-occurrences)
                          (when parent
                            (set-parent-link! g (term-uplink (graph-term g cloned)) parent))
                          #t))))))))))))

  ;; Count the body's terms and calls.  `Fix' member bodies are separate
  ;; procedures and are not part of this function's size.
  (define (scan-body! g link scan)
    (let ([term (read-term-link g link)])
      (when term
        (scan-terms-set! scan (+ 1 (scan-terms scan)))
        (let ([kind (term-kind (graph-term g term))])
          (cond
            [(k-let-val? kind)
              (scan-exprs-set! scan (+ 1 (scan-exprs scan)))
              (let ([expr (read-expr-link g (k-let-val-expr kind))])
                (when (and expr (k-prim-call? (expr-kind (graph-expr g expr))))
                  (scan-exprs-set! scan (+ 1 (scan-exprs scan)))))
              (scan-body! g (k-let-val-body kind) scan)]
            [(k-letk? kind)
              (for-each
                (lambda (function)
                  (when (function-is-rec (graph-function g function))
                    (scan-has-loop-set! scan #t))
                  (scan-body! g (function-body (graph-function g function)) scan))
                (live-functions g (k-letk-functions kind)))
              (scan-body! g (k-letk-body kind) scan)]
            [(k-fix? kind)
              (let ([calls (scan-calls scan)] [has-loop (scan-has-loop scan)])
                (for-each
                  (lambda (function)
                    (scan-body! g (function-body (graph-function g function)) scan))
                  (live-functions g (k-fix-functions kind)))
                (scan-calls-set! scan calls)
                (scan-has-loop-set! scan has-loop))
              (scan-body! g (k-fix-body kind) scan)]
            [(k-if? kind)
              (scan-body! g (k-if-then kind) scan)
              (scan-body! g (k-if-else kind) scan)]
            [(k-app? kind) (scan-calls-set! scan (+ 1 (scan-calls scan)))]
            [else (if #f #f)])))))

  (define (all-uses-are-calls? g binder)
    (let ([ok #t])
      (for-each-occurrence g binder
        (lambda (occ)
          (when ok
            (let ([term (read-term-link g (free-owner g occ))])
              (if (not term)
                (set! ok #f)
                (let ([kind (term-kind (graph-term g term))])
                  (unless (and (k-app? kind) (eqv? (k-app-func kind) occ))
                    (set! ok #f))))))))
      ok))

  ;; List of (function-id . function-link) in traversal order.
  (define (collect-functions g link)
    (let ([out '()])
      (let walk ([link link])
        (let ([term (read-term-link g link)])
          (when term
            (let ([kind (term-kind (graph-term g term))])
              (cond
                [(k-let-val? kind) (walk (k-let-val-body kind))]
                [(or (k-letk? kind) (k-fix? kind))
                  (let ([functions (if (k-letk? kind) (k-letk-functions kind) (k-fix-functions kind))]
                        [body (if (k-letk? kind) (k-letk-body kind) (k-fix-body kind))])
                    (for-each
                      (lambda (flink)
                        (let ([function (read-function-link g flink)])
                          (when function
                            (set! out (cons (cons function flink) out))
                            (walk (function-body (graph-function g function))))))
                      (function-links-slice g functions))
                    (walk body))]
                [(k-if? kind)
                  (walk (k-if-then kind))
                  (walk (k-if-else kind))]
                [else (if #f #f)])))))
      (reverse out)))

  (define (collect-call-sites! g link caller out)
    (let ([term (read-term-link g link)])
      (when term
        (let ([kind (term-kind (graph-term g term))])
          (cond
            [(k-let-val? kind) (collect-call-sites! g (k-let-val-body kind) caller out)]
            [(or (k-letk? kind) (k-fix? kind))
              (let ([functions (if (k-letk? kind) (k-letk-functions kind) (k-fix-functions kind))]
                    [body (if (k-letk? kind) (k-letk-body kind) (k-fix-body kind))])
                (for-each
                  (lambda (function)
                    ;; A continuation is a block of the function around it: a
                    ;; call inside one is a call made by that function.
                    (let ([owner (if (function-cont (graph-function g function)) function caller)])
                      (collect-call-sites! g (function-body (graph-function g function)) owner out)))
                  (live-functions g functions))
                (collect-call-sites! g body caller out))]
            [(k-if? kind)
              (collect-call-sites! g (k-if-then kind) caller out)
              (collect-call-sites! g (k-if-else kind) caller out)]
            [(k-app? kind)
              (let ([binder (free-binder-rooted g (k-app-func kind))])
                (hashtable-set! out binder
                  (cons (make-call-site link caller) (hashtable-ref out binder '()))))]
            [else (if #f #f)])))))

  ;; Binders that participate in a call-graph cycle (self or mutual recursion;
  ;; a lone self edge is a component of size 1 and is not reported here, the
  ;; caller handles self calls separately).  An edge runs from the function
  ;; making a call to the function it calls.  Returns an entity-set of binders.
  (define (recursive-binders g infos binder-order)
    (let ([index (make-eqv-hashtable)]
          [binders '()]
          [count 0]
          [edges '()])
      (define (node-of! binder)
        (or (hashtable-ref index binder #f)
            (let ([n count])
              (hashtable-set! index binder n)
              (set! binders (cons binder binders))
              (set! count (+ count 1))
              n)))
      (for-each (lambda (binder) (node-of! binder)) binder-order)
      (for-each
        (lambda (binder)
          (let* ([info (hashtable-ref infos binder #f)]
                 [to (node-of! binder)])
            (for-each
              (lambda (site)
                (let ([caller (call-site-caller site)])
                  (when caller
                    (let ([from (node-of! (function-binder-rooted g (function-var (graph-function g caller))))])
                      (set! edges (cons (cons from to) edges))))))
              (function-info-call-sites info))))
        binder-order)
      (let ([out-adj (make-vector count '())]
            [in-adj (make-vector count '())]
            [node-binder (list->vector (reverse binders))]
            [recursive (make-entity-set)])
        (for-each
          (lambda (edge)
            (vector-set! out-adj (car edge) (cons (cdr edge) (vector-ref out-adj (car edge))))
            (vector-set! in-adj (cdr edge) (cons (car edge) (vector-ref in-adj (cdr edge)))))
          (reverse edges))
        (for-each
          (lambda (component)
            (when (> (length component) 1)
              (for-each (lambda (node) (entity-set-insert! recursive (vector-ref node-binder node)))
                component)))
          (kosaraju-scc count out-adj in-adj))
        recursive))))

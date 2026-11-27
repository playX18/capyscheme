;; The graph-CPS optimizer: worklist driver, passes, and SSA conversion.
;;
;; Port of crates/capy/src/compiler/cps/optimize.rs.
;;
;; API:
;;   (optimize-graph-func-to-ssa program dump-graph?) => optimized-program
;;       PROGRAM is a <graph-function-program> (core compiler cps convert).
;;       Mirrors optimize_graph_func_to_ssa: optimize the graph, reify, merge
;;       nested groups, run the stage/flow analyses, lower to CFG/SSA.
;;       (`ctx' is dropped.)  The result is a record:
;;         (optimized-program-ssa p)        the cfg program (lower-graph result)
;;         (optimized-program-graph-cps p)  the optimized-graph dump string when
;;                                          DUMP-GRAPH? was true, else #f
;;         (optimized-program-stats p)      an optimization-stats
;;   (optimize-graph graph entry root gas) => optimization-stats
;;       GAS is a fixnum or #f for unlimited.
;;   default-gas  (= 42000)
;;   optimization-stats: (optimization-stats-ref stats 'name) with names
;;       iterations gas-used ran-out-of-gas dead-bindings-processed
;;       dead-letvals-removed eta-continuations eta-functions
;;       constant-branches-simplified identical-branches-simplified
;;       known-prim-propagations singleton-calls-inlined
;;       singleton-continuations-inlined contifications scc-contifications
;;       dom-contifications contified-functions scc-contified-functions
;;       dom-contified-functions loops-introduced loop-self-calls
;;       loops-accumulated accumulated-self-calls
;;
;; Calls made to the other pipeline libraries (exact arguments):
;;   (reify-graph! graph entry)                 => reify-info
;;   (merge-nested-groups! graph root-link)     root-link = entry body link
;;   (make-stage-analysis graph (reify-info-functions reify))
;;   (flow-analysis graph reify)
;;   (lower-graph graph reify #f)               share plan: not ported (the
;;                                              Rust sharing call is `false &&')
;;
;; Not ported (no effect on the compiled output):
;;   * ProfileScope instrumentation (compiler.lower.gcps.*): the driver wraps
;;     phases itself.
;;   * The CAPY_GCPS_DUMP_CONTIFY dump files (dump_contification and friends).
;;   * verbose_log! tracing.
;;   * analyze_sharing / share.rs (hard-disabled in Rust).
;;
;; Flags are read from the environment (CAPY_GCPS_CONTIFY, CAPY_GCPS_INLINE,
;; CAPY_GCPS_INLINE_MAX, CAPY_GCPS_INLINE_SMALL, CAPY_GCPS_INLINE_PRODUCT,
;; CAPY_GCPS_LOOPS, CAPY_GCPS_ACCUMULATE) with the same defaults as flags.rs.
;;
;; Determinism: the OptimizerState SecondaryMaps are eqv hashtables keyed by
;; fixnum ids, used only for lookup; no table is ever iterated.  The worklist
;; queues and the pending-function-binder set come from
;; (core compiler cps worklist), which keeps Rust's orders.
(library (core compiler cps optimize)
  (export
    optimize-graph-func-to-ssa optimize-graph default-gas
    optimized-program? optimized-program-ssa optimized-program-graph-cps
    optimized-program-stats
    optimization-stats? optimization-stats-ref)
  (import (core) (capy)
    (core compiler cps graph)
    (only (core compiler cps convert) graph-function-program-graph
      graph-function-program-entry graph-function-program-root)
    (core compiler cps worklist)
    (core compiler cps fold)
    (core compiler cps inline)
    (core compiler cps introduce-loops)
    (prefix (core compiler cps scc-contify) scc:)
    (prefix (core compiler cps dom-contify) dom:)
    (core compiler cps pretty)
    (core compiler cps reify)
    (core compiler cps fixmerge)
    (core compiler cps flow)
    (core compiler cps analysis)
    (core compiler cfg lower))

  (define default-gas 42000)

  ;; Run the stage and flow analyses the Rust pipeline runs before the (disabled)
  ;; sharing analysis.  Their results are not used by lower-graph.
  (define run-sharing-analyses? #t)

 
  (define stat-names
    '(iterations gas-used ran-out-of-gas dead-bindings-processed dead-letvals-removed
      eta-continuations eta-functions constant-branches-simplified
      identical-branches-simplified known-prim-propagations singleton-calls-inlined
      singleton-continuations-inlined contifications scc-contifications
      dom-contifications contified-functions scc-contified-functions
      dom-contified-functions loops-introduced loop-self-calls loops-accumulated
      accumulated-self-calls))

  (define-record-type (optimization-stats make-optimization-stats optimization-stats?)
    (fields values))

  (define (new-stats) (make-optimization-stats (make-vector (length stat-names) 0)))

  (define (stat-index name)
    (let loop ([names stat-names] [i 0])
      (cond
        [(null? names) (assertion-violation 'optimization-stats-ref "unknown stat" name)]
        [(eq? (car names) name) i]
        [else (loop (cdr names) (+ i 1))])))

  (define (optimization-stats-ref stats name)
    (vector-ref (optimization-stats-values stats) (stat-index name)))
  (define (stat-set! stats name v)
    (vector-set! (optimization-stats-values stats) (stat-index name) v))
  (define (stat-add! stats name n)
    (stat-set! stats name (+ n (optimization-stats-ref stats name))))
  (define (stat-inc! stats name) (stat-add! stats name 1))

  (define-record-type (optimized-program make-optimized-program optimized-program?)
    (fields ssa stats graph-cps))

  ;; ContifyMode: 'Off 'Scc 'Dom 'DomThenScc
  (define (current-contify-mode)
    (let ([w (flag-enum "CAPY_GCPS_CONTIFY" "dom"
               '("0" "off" "none" "false" "scc" "legacy" "both" "dom+scc" "dom-then-scc"
                 "dom" "dominator" "dominators"))])
      (cond
        [(member w '("off" "0" "none" "false")) 'Off]
        [(member w '("scc" "legacy")) 'Scc]
        [(member w '("both" "dom+scc" "dom-then-scc")) 'DomThenScc]
        [else 'Dom])))

  (define (current-inline-flag)
    (flag-enum "CAPY_GCPS_INLINE" "all"
      '("0" "off" "none" "false" "leaf" "known" "nonrecursive" "both" "all" "1" "on" "true")))


  (define (mref ht k) (hashtable-ref ht k #f))
  (define (mset! ht k v) (hashtable-set! ht k v))

  (define (kind-of g term) (term-kind (graph-term g term)))
  (define (uplink-of g term) (term-uplink (graph-term g term)))
  (define (parent-of g term) (read-parent-link g (uplink-of g term)))
  (define (fn g f) (graph-function g f))

  (define (set-copy s)
    (let ([c (make-entity-set)])
      (for-each (lambda (x) (entity-set-insert! c x)) (entity-set->list s))
      c))
  (define (set-extend! s ids) (for-each (lambda (x) (entity-set-insert! s x)) ids))

  ;; The links of FUNCTIONS paired with their live functions: list of
  ;; (link . function).
  (define (live-function-links g functions)
    (let loop ([links (function-links-slice g functions)] [acc '()])
      (if (null? links)
        (reverse acc)
        (let ([function (read-function-link g (car links))])
          (loop (cdr links) (if function (cons (cons (car links) function) acc) acc))))))

  ;; Fix / Letk accessors
  (define (kind-functions kind) (if (k-fix? kind) (k-fix-functions kind) (k-letk-functions kind)))
  (define (kind-fix-body kind) (if (k-fix? kind) (k-fix-body kind) (k-letk-body kind)))
  (define (group-kind? kind) (or (k-fix? kind) (k-letk? kind)))

  (define-record-type (optimizer-state make-optimizer-state optimizer-state?)
    (fields
      worklist
      known-functions            ; binder -> function link
      function-defs              ; binder -> subterm
      local-function-binders     ; binder -> #t
      function-owner-defs        ; function id -> subterm
      return-cont-owners         ; binder -> function id
      known-exprs                ; binder -> subexpr
      known-expr-defs            ; binder -> subterm
      dead-bindings              ; worklist queue of binders
      stats))

  (define (new-optimizer-state)
    (make-optimizer-state
      (make-graph-worklist)
      (make-eqv-hashtable) (make-eqv-hashtable) (make-eqv-hashtable)
      (make-eqv-hashtable) (make-eqv-hashtable) (make-eqv-hashtable)
      (make-eqv-hashtable)
      (make-worklist-queue)
      (new-stats)))

  (define (st-known-function st b) (mref (optimizer-state-known-functions st) b))
  (define (st-function-def st b) (mref (optimizer-state-function-defs st) b))
  (define (st-local-function-binder? st b) (mref (optimizer-state-local-function-binders st) b))
  (define (st-function-owner-def st f) (mref (optimizer-state-function-owner-defs st) f))
  (define (st-return-cont-owner st b) (mref (optimizer-state-return-cont-owners st) b))
  (define (st-known-expr st b) (mref (optimizer-state-known-exprs st) b))
  (define (st-known-expr-def st b) (mref (optimizer-state-known-expr-defs st) b))
  (define (st-add-subterm! st link) (graph-worklist-add-subterm! (optimizer-state-worklist st) link))
  (define (st-stats st) (optimizer-state-stats st))

  (define (optimize-graph g entry root gas)
    (let ([loops (if (gcps-loops?) (introduce-loops g entry) (zero-loop-stats))]
          [st (new-optimizer-state)]
          [contify-mode (current-contify-mode)])
      (collect-redexes! st g root)
      (run! st g (or gas #f) contify-mode)
      (let* ([stats (st-stats st)]
             [loops
              ;; Simplification and contification expose new self tail calls.
              (if (gcps-loops?)
                (let ([more (introduce-loops g entry)])
                  (make-loop-stats
                    (+ (loop-stats-loops loops) (loop-stats-loops more))
                    (+ (loop-stats-self-calls loops) (loop-stats-self-calls more))
                    (+ (loop-stats-loops-accumulated loops) (loop-stats-loops-accumulated more))
                    (+ (loop-stats-accumulated-self-calls loops)
                       (loop-stats-accumulated-self-calls more))))
                loops)])
        (stat-set! stats 'loops-introduced (loop-stats-loops loops))
        (stat-set! stats 'loop-self-calls (loop-stats-self-calls loops))
        (stat-set! stats 'loops-accumulated (loop-stats-loops-accumulated loops))
        (stat-set! stats 'accumulated-self-calls (loop-stats-accumulated-self-calls loops))
        stats)))

  ;; optimize_graph_program: inline, optimize, inline again.  Returns stats.
  (define (phase-timed name thunk)
    (let ([flag (getenv "CAPY_PROFILE_PASSES")])
      (if (and flag (not (equal? flag "")))
        (let ([t0 (microsecond)])
          (call-with-values thunk
            (lambda results
              (display (string-append ";; PERF (capy) phase=" name " elapsed_ms="
                         (number->string (quotient (- (microsecond) t0) 1000)) "\n")
                (current-error-port))
              (apply values results))))
        (thunk))))

  (define (optimize-graph-program! program)
    (let* ([g (graph-function-program-graph program)]
           [entry (graph-function-program-entry program)]
           [root (graph-function-program-root program)]
           [inline-mode (inline-mode-from-flag (current-inline-flag))]
           [inline-max (flag-usize "CAPY_GCPS_INLINE_MAX" 40 1)]
           [inline-small (flag-usize "CAPY_GCPS_INLINE_SMALL" 60 0)]
           [inline-product (flag-usize "CAPY_GCPS_INLINE_PRODUCT" 320 0)])
      (unless (eq? inline-mode 'Off)
        (phase-timed "scheme.opt.inline1"
          (lambda () (inline-graph g entry inline-mode inline-max inline-small inline-product))))
      (let ([stats (phase-timed "scheme.opt.main" (lambda () (optimize-graph g entry root default-gas)))])
        (when (memq inline-mode '(Known Both))
          (phase-timed "scheme.opt.inline2"
            (lambda () (inline-graph g entry inline-mode inline-max inline-small inline-product))))
        stats)))

  (define (optimize-graph-func-to-ssa program dump-graph?)
    (let* ([stats (optimize-graph-program! program)]
           [g (graph-function-program-graph program)]
           [entry (graph-function-program-entry program)]
           [graph-cps
            (and dump-graph?
              (let ([root (read-term-link g (graph-function-program-root program))])
                (unless root (error 'optimize-graph-func-to-ssa "graph root is dead"))
                (render-graph g root)))]
           ;; Reify before group merging (the merge needs `is-reified').  The
           ;; merge changes no bindings or uses, so the reify info stays valid.
           [reify (phase-timed "scheme.opt.reify" (lambda () (reify-graph! g entry)))])
      ;; Merge adjacent Fix/Letk groups so lambdas and reified continuations
      ;; bound at the same point share one closure-creation site.
      (phase-timed "scheme.opt.merge" (lambda () (merge-nested-groups! g (graph-function-program-root program))))
      (when run-sharing-analyses?
        (phase-timed "scheme.opt.stage" (lambda () (make-stage-analysis g (reify-info-functions reify))))
        (phase-timed "scheme.opt.flow" (lambda () (flow-analysis g reify))))
      (let ([ssa (phase-timed "scheme.opt.lower" (lambda () (lower-graph g reify #f)))])
        (make-optimized-program ssa stats graph-cps))))

  (define (run! st g gas contify-mode)
    (let* ([stats (st-stats st)]
           [initial-gas (or gas #f)]
           [remaining gas])
      ;; REMAINING is #f for unlimited gas.
      (let loop ()
        (when (or (not remaining) (> remaining 0))
          (when remaining (set! remaining (- remaining 1)))
          (process-dead-bindings! st g)
          (let ([next (graph-worklist-get-next-live-subterm! (optimizer-state-worklist st) g)])
            (when next
              (let* ([queued-link (car next)]
                     [term (cadr next)]
                     [parent (caddr next)]
                     [active-link (if (not parent) queued-link (get-active-link-for g term))])
                (when active-link
                  (stat-inc! stats 'iterations)
                  (let ([kind (kind-of g term)])
                    (cond
                      [(k-let-val? kind)
                        (reduce-letval! st g queued-link active-link term
                          (k-let-val-binder kind) (k-let-val-expr kind) (k-let-val-body kind))]
                      [(k-app? kind)
                        (reduce-call! st g queued-link active-link term
                          (k-app-func kind) (k-app-vars kind) (k-app-cont kind))]
                      [(k-continue? kind)
                        (reduce-continue! st g queued-link active-link term
                          (k-continue-cont kind) (k-continue-vars kind))]
                      [(k-letk? kind)
                        (reduce-eta-continuations! st g (k-letk-functions kind))]
                      [(k-fix? kind)
                        (reduce-eta-functions! st g (k-fix-functions kind))
                        (try-contify-fix! st g active-link term (k-fix-functions kind)
                          (k-fix-body kind) contify-mode)]
                      [(k-if? kind)
                        (reduce-if! st g queued-link active-link term
                          (k-if-test kind) (k-if-then kind) (k-if-else kind))]
                      [else (if #f #f)])))
                (loop))))))
      (when (and remaining (= remaining 0))
        (stat-set! stats 'ran-out-of-gas #t))
      (stat-set! stats 'gas-used (if gas (max 0 (- initial-gas remaining)) 0))))

  (define (collect-redexes! st g root)
    (let ([term (read-term-link g root)])
      (when term
        (let ([kind (kind-of g term)])
          (when (or (k-let-val? kind) (k-app? kind) (k-continue? kind) (k-if? kind) (k-fix? kind))
            (st-add-subterm! st root))
          (when (k-let-val? kind)
            (let ([binder (k-let-val-binder kind)])
              (mset! (optimizer-state-known-exprs st) binder (k-let-val-expr kind))
              (mset! (optimizer-state-known-expr-defs st) binder root)))
          (when (group-kind? kind)
            (for-each
              (lambda (link)
                (let ([function (read-function-link g link)])
                  (when function
                    (let ([binder (function-var (fn g function))])
                      (mset! (optimizer-state-known-functions st) binder link)
                      (mset! (optimizer-state-function-defs st) binder root)
                      (mset! (optimizer-state-local-function-binders st) binder #t)
                      (mset! (optimizer-state-function-owner-defs st) function root)
                      (let ([return-cont (function-cont (fn g function))])
                        (when return-cont
                          (mset! (optimizer-state-local-function-binders st) return-cont #t)
                          (mset! (optimizer-state-return-cont-owners st) return-cont function)))))))
              (function-links-slice g (kind-functions kind))))
          (when (and (k-letk? kind)
                     (has-eta-continuation-candidate? st g (k-letk-functions kind)))
            (st-add-subterm! st root))
          (for-each-subterm g term (lambda (child) (collect-redexes! st g child)))
          (when (group-kind? kind)
            (for-each
              (lambda (link)
                (let ([function (read-function-link g link)])
                  (when function
                    (collect-redexes! st g (function-body (fn g function))))))
              (function-links-slice g (kind-functions kind))))))))

  (define (process-dead-bindings! st g)
    (let ([q (optimizer-state-dead-bindings st)])
      (let loop ()
        (let ([binding (if (worklist-queue-empty? q) worklist-queue-none (worklist-queue-get! q))])
          (unless (eq? binding worklist-queue-none)
            (stat-inc! (st-stats st) 'dead-bindings-processed)
            (let ([expr (st-known-expr st binding)])
              (when (and expr (expr-link-is-dead-removable? g expr))
                (let ([expr-def (st-known-expr-def st binding)])
                  (take-known-expr! st binding)
                  (when expr-def (st-add-subterm! st expr-def)))))
            (clear-known-function! st g binding)
            (loop))))))

  ;; => the cleared link or #f
  (define (clear-known-function! st g binding)
    (let ([link (st-known-function st binding)])
      (and link
        (begin
          (mset! (optimizer-state-known-functions st) binding #f)
          (mset! (optimizer-state-function-defs st) binding #f)
          (let ([function (read-function-link g link)])
            (when function
              (mset! (optimizer-state-function-owner-defs st) function #f)
              (let ([return-cont (function-cont (fn g function))])
                (when return-cont
                  (mset! (optimizer-state-return-cont-owners st) return-cont #f)))))
          (clear-function-link! g link)
          link))))

  (define (take-known-expr! st binding)
    (let ([expr (st-known-expr st binding)])
      (when expr
        (mset! (optimizer-state-known-exprs st) binding #f)
        (mset! (optimizer-state-known-expr-defs st) binding #f))
      expr))

  ;; The literal bound to BINDING, or `fold-none'.
  (define (known-literal-value st g binding)
    (let ([link (st-known-expr st binding)])
      (if (not link)
        fold-none
        (let ([expr (read-expr-link g link)])
          (if (not expr)
            fold-none
            (let ([kind (expr-kind (graph-expr g expr))])
              (if (k-literal? kind) (k-literal-value kind) fold-none)))))))

  (define (kill-free-vars-of-expr-link! st g expr-link)
    (let ([expr (read-expr-link g expr-link)])
      (when expr
        (for-each (lambda (occ) (kill-occurrence! st g occ))
          (push-free-vars-of-expr g expr '())))))

  (define (replace-after-queue! g queued-link active-link term)
    (unless (eqv? queued-link active-link)
      (set-term-link! g queued-link (or (read-term-link g active-link) term))))

  (define (reduce-letval! st g queued-link active-link term binder expr body)
    (if (not (binder-is-dead? g binder))
      (reduce-known-primcall! st g binder expr)
      (when (expr-link-is-dead-removable? g expr)
        (stat-inc! (st-stats st) 'dead-letvals-removed)
        (when (take-known-expr! st binder)
          (kill-free-vars-of-expr-link! st g expr))
        (replace-with-existing-body! st g active-link term body)
        (st-add-subterm! st active-link)
        (replace-after-queue! g queued-link active-link term))))

  (define (expr-link-is-dead-removable? g link)
    (let ([expr (read-expr-link g link)])
      (and expr (k-literal? (expr-kind (graph-expr g expr))))))

  (define (reduce-known-primcall! st g binder expr-link)
    (let ([expr-id (read-expr-link g expr-link)])
      (when expr-id
        (let ([kind (expr-kind (graph-expr g expr-id))])
          (when (k-prim-call? kind)
            (let* ([prim (k-prim-call-prim kind)]
                   [arg-occurrences (free-vars-slice g (k-prim-call-vars kind))])
              (let loop ([occs arg-occurrences] [values '()] [arg-binders '()])
                (if (pair? occs)
                  (let* ([arg-binder (free-binder g (car occs))]
                         [value (known-literal-value st g arg-binder)])
                    (unless (fold-none? value)
                      (loop (cdr occs) (cons value values) (cons arg-binder arg-binders))))
                  (let ([value (try-fold-values prim (reverse values))])
                    (unless (fold-none? value)
                      (stat-inc! (st-stats st) 'known-prim-propagations)
                      (for-each (lambda (occ) (kill-occurrence! st g occ)) arg-occurrences)
                      (for-each
                        (lambda (arg-binder)
                          (when (binder-is-dead? g arg-binder)
                            (let ([def (st-known-expr-def st arg-binder)])
                              (when def (st-add-subterm! st def)))))
                        (reverse arg-binders))
                      (expr-kind-set! (graph-expr g expr-id) (make-k-literal value))
                      (enqueue-occurrence-owners! st g binder)))))))))))

  (define (reduce-if! st g queued-link active-link term test consequent alternative)
    (let ([value (known-literal-value st g (free-binder g test))])
      (cond
        [(not (fold-none? value))
          (stat-inc! (st-stats st) 'constant-branches-simplified)
          (if value
            (replace-if-with-branch! st g queued-link active-link term consequent alternative)
            (replace-if-with-branch! st g queued-link active-link term alternative consequent))]
        [(direct-continue-branches-equivalent? g consequent alternative)
          (stat-inc! (st-stats st) 'identical-branches-simplified)
          (replace-if-with-branch! st g queued-link active-link term consequent alternative)])))

  (define (replace-if-with-branch! st g queued-link active-link term taken skipped)
    (let* ([parent (parent-of g term)]
           [parent-term-id (and parent (parent-term? parent) (parent-id parent))]
           [same-replacement (eqv? (read-term-link g taken) (read-term-link g skipped))])
      (replace-with-existing-body! st g active-link term taken)
      (cond
        [(not same-replacement) (kill-free-vars-of-term-subtree-link! st g skipped)]
        [(not (eqv? skipped taken)) (clear-term-link! g skipped)])
      (when parent-term-id
        (let ([link (get-active-link-for g parent-term-id)])
          (when link (st-add-subterm! st link))))
      (st-add-subterm! st active-link)
      (replace-after-queue! g queued-link active-link term)))

  (define (direct-continue-branches-equivalent? g left right)
    (let ([l (read-term-link g left)] [r (read-term-link g right)])
      (and l r
        (let ([lk (kind-of g l)] [rk (kind-of g r)])
          (and (k-continue? lk) (k-continue? rk)
            (eqv? (free-binder g (k-continue-cont lk)) (free-binder g (k-continue-cont rk)))
            (free-vars-have-same-binders? g (k-continue-vars lk) (k-continue-vars rk)))))))

  (define (reduce-eta-continuations! st g functions)
    (for-each
      (lambda (link)
        (let ([function (read-function-link g link)])
          (when function
            (let ([target (eta-continuation-target st g function)])
              (when target
                (contract-eta-function! st g link function target #t))))))
      (function-links-slice g functions)))

  (define (has-eta-continuation-candidate? st g functions)
    (exists
      (lambda (link)
        (let ([function (read-function-link g link)])
          (and function (eta-continuation-target st g function) #t)))
      (function-links-slice g functions)))

  (define (reduce-eta-functions! st g functions)
    (for-each
      (lambda (link)
        (let ([function (read-function-link g link)])
          (when function
            (let ([target (eta-function-target st g function)])
              (when target
                (contract-eta-function! st g link function target #f))))))
      (function-links-slice g functions)))

  ;; => target binder or #f
  (define (eta-continuation-target st g function)
    (let ([data (fn g function)])
      (and (not (function-cont data))
           (not (function-variadic data))
           (not (function-is-cold data))
           (not (function-is-noinline data))
           (not (function-is-reified data))
           (let ([body (read-term-link g (function-body data))])
             (and body
               (let ([kind (kind-of g body)])
                 (and (k-continue? kind)
                   (let ([target (free-binder g (k-continue-cont kind))]
                         [args (k-continue-vars kind)])
                     (and (not (eqv? target (function-var data)))
                          (not (binder-has-local-definition? st target))
                          (args-are-formals-in-order? g args (function-vars data))
                          (all-occurrences-are-control-uses-available-at? st g (function-var data) target)
                          target)))))))))

  (define (eta-function-target st g function)
    (let* ([data (fn g function)] [return-cont (function-cont data)])
      (and return-cont
           (not (function-variadic data))
           (not (function-is-cold data))
           (not (function-is-noinline data))
           (not (function-is-reified data))
           (let ([body (read-term-link g (function-body data))])
             (and body
               (let ([kind (kind-of g body)])
                 (and (k-app? kind)
                   (let ([target (free-binder g (k-app-func kind))]
                         [args (k-app-vars kind)]
                         [cont (k-app-cont kind)])
                     (and (not (eqv? target (function-var data)))
                          (not (binder-has-local-definition? st target))
                          (eqv? (free-binder g cont) return-cont)
                          (args-are-formals-in-order? g args (function-vars data))
                          (all-occurrences-are-callee-uses-available-at? st g (function-var data) target)
                          target)))))))))

  (define (binder-has-local-definition? st binder)
    (or (st-local-function-binder? st binder)
        (and (st-function-def st binder) #t)
        (and (st-return-cont-owner st binder) #t)))

  (define (args-are-formals-in-order? g args formals)
    (let ([args (free-vars-slice g args)] [formals (bound-vars-slice g formals)])
      (and (= (length args) (length formals))
           (let loop ([a args] [f formals])
             (or (null? a)
                 (and (eqv? (free-binder g (car a)) (car f))
                      (loop (cdr a) (cdr f))))))))

  (define (free-vars-have-same-binders? g left right)
    (let ([left (free-vars-slice g left)] [right (free-vars-slice g right)])
      (and (= (length left) (length right))
           (let loop ([a left] [b right])
             (or (null? a)
                 (and (eqv? (free-binder g (car a)) (free-binder g (car b)))
                      (loop (cdr a) (cdr b))))))))

  (define (all-occurrences-are-control-uses-available-at? st g removed replacement)
    (all-occurrences-match-available-use? st g removed replacement
      (lambda (kind occ)
        (or (and (k-continue? kind) (eqv? (k-continue-cont kind) occ))
            (and (k-app? kind) (eqv? (k-app-cont kind) occ))
            (and (k-branch-prim? kind)
                 (or (eqv? (k-branch-prim-consequent kind) occ)
                     (eqv? (k-branch-prim-alternative kind) occ)))))))

  (define (all-occurrences-are-callee-uses-available-at? st g removed replacement)
    (all-occurrences-match-available-use? st g removed replacement
      (lambda (kind occ) (and (k-app? kind) (eqv? (k-app-func kind) occ)))))

  (define (all-occurrences-match-available-use? st g removed replacement occurrence-matches?)
    (let ([allowed #t])
      (for-each-occurrence g removed
        (lambda (occ)
          (when allowed
            (let* ([owner (free-owner g occ)]
                   [term (read-term-link g owner)])
              (if (not term)
                (set! allowed #f)
                (set! allowed
                  (and (not (term-is-inside-any-function? g term))
                       (occurrence-matches? (kind-of g term) occ)
                       (binder-is-available-at-term? st g term replacement))))))))
      allowed))

  (define (contract-eta-function! st g link function target is-continuation)
    (let ([binding (function-var (fn g function))])
      (subst-var-for-binders! g target binding)
      (kill-direct-free-vars-of-term-link! st g (function-body (fn g function)))
      (if (eqv? (st-known-function st binding) link)
        (clear-known-function! st g binding)
        (clear-function-link! g link))
      (enqueue-occurrence-owners! st g target)
      (stat-inc! (st-stats st) (if is-continuation 'eta-continuations 'eta-functions))))

  (define (kill-direct-free-vars-of-term-link! st g link)
    (let ([term (read-term-link g link)])
      (when term
        (for-each (lambda (occ) (kill-occurrence! st g occ))
          (push-direct-free-vars-of-term g term '())))))

  (define (kill-free-vars-of-term-subtree-link! st g link)
    (let ([term (read-term-link g link)])
      (when term
        (let ([kind (kind-of g term)])
          (cond
            [(k-let-val? kind)
              (take-known-expr! st (k-let-val-binder kind))
              (kill-free-vars-of-expr-link! st g (k-let-val-expr kind))
              (kill-free-vars-of-term-subtree-link! st g (k-let-val-body kind))]
            [(group-kind? kind)
              (let ([function-bodies
                     (let loop ([links (function-links-slice g (kind-functions kind))] [acc '()])
                       (if (null? links)
                         (reverse acc)
                         (let ([function (read-function-link g (car links))])
                           (loop (cdr links)
                             (if function
                               (cons (cons (function-var (fn g function)) (function-body (fn g function)))
                                 acc)
                               acc)))))])
                (for-each
                  (lambda (pair)
                    (clear-known-function! st g (car pair))
                    (kill-free-vars-of-term-subtree-link! st g (cdr pair)))
                  function-bodies)
                (kill-free-vars-of-term-subtree-link! st g (kind-fix-body kind)))]
            [(k-if? kind)
              (kill-direct-free-vars-of-term-link! st g link)
              (kill-free-vars-of-term-subtree-link! st g (k-if-then kind))
              (kill-free-vars-of-term-subtree-link! st g (k-if-else kind))]
            [else (kill-direct-free-vars-of-term-link! st g link)])))))

  (define (enqueue-occurrence-owners! st g binder)
    (for-each (lambda (occ) (st-add-subterm! st (free-owner g occ)))
      (collect-occurrences g binder)))

  ;; => (binder link function) or #f
  (define (classify-function st g callee)
    (let* ([binder (free-binder g callee)]
           [link (st-known-function st binder)]
           [function (and link (read-function-link g link))])
      (and function (list binder link function))))

  (define (reduce-call! st g queued-link active-link term callee args cont)
    (let ([c (classify-function st g callee)])
      (when c
        (let* ([binder (car c)] [function (caddr c)] [data (fn g function)])
          (unless (or (not (function-cont data))
                      (function-is-rec data)
                      (function-variadic data)
                      (not (binder-is-singleton-or-dead? g binder)))
            (when (arity-matches? g function args)
              (stat-inc! (st-stats st) 'singleton-calls-inlined)
              (beta-reduce-call! st g function args cont)
              (replace-with-existing-body! st g active-link term (function-body data))
              (kill-binding! st g callee)
              (graph-worklist-add-occurrences! (optimizer-state-worklist st) g
                (free-vars-slice g args))
              (replace-after-queue! g queued-link active-link term)))))))

  (define (reduce-continue! st g queued-link active-link term cont args)
    (let ([c (classify-function st g cont)])
      (when c
        (let* ([binder (car c)] [function (caddr c)] [data (fn g function)])
          (unless (or (function-cont data)
                      (function-is-rec data)
                      (function-variadic data)
                      (function-is-noinline data)
                      (function-is-cold data)
                      (not (binder-is-singleton-or-dead? g binder)))
            (when (arity-matches? g function args)
              (when (continuation-body-is-simple-inline? st g function)
                (stat-inc! (st-stats st) 'singleton-continuations-inlined)
                (beta-reduce-call! st g function args #f)
                (replace-with-existing-body! st g active-link term (function-body data))
                (kill-binding! st g cont)
                (enqueue-known-occurrences! st g function args)
                (replace-after-queue! g queued-link active-link term))))))))

  (define (arity-matches? g function args)
    (let* ([data (fn g function)]
           [fixed (length (bound-vars-slice g (function-vars data)))]
           [actual (length (free-vars-slice g args))])
      (if (function-variadic data) (>= actual fixed) (= actual fixed))))

  ;; CONT: the call's return continuation occurrence, or #f.
  (define (beta-reduce-call! st g function args cont)
    (let* ([data (fn g function)]
           [formals (bound-vars-slice g (function-vars data))]
           [actuals (free-vars-slice g args)])
      (let loop ([a actuals] [f formals])
        (when (and (pair? a) (pair? f))
          (subst-var-for-bound! g (car a) (car f))
          (loop (cdr a) (cdr f))))
      (when (and cont (function-cont data))
        (collect-redexes-using-fn-ret-cont! st g (function-cont data))
        (subst-var-for-bound! g cont (function-cont data)))))

  (define (replace-with-existing-body! st g active-link old-term replacement-link)
    (let ([replacement (read-term-link g replacement-link)])
      (when replacement
        (replace-term-with! st g active-link old-term replacement)
        (unless (eqv? replacement-link active-link)
          (clear-term-link! g replacement-link)))))

  (define (replace-term-with! st g active-link old-term replacement)
    (let* ([old-occurrences (push-direct-free-vars-of-term g old-term '())]
           [new-occurrences (push-direct-free-vars-of-term g replacement '())]
           [occurrence-set (make-entity-set)])
      (set-extend! occurrence-set new-occurrences)
      (set-term-link! g active-link replacement)
      (for-each
        (lambda (occ)
          (unless (entity-set-contains? occurrence-set occ)
            (kill-occurrence! st g occ)))
        old-occurrences)
      (for-each (lambda (occ) (set-free-owner! g occ active-link)) new-occurrences)
      (let ([parent (parent-of g old-term)])
        (if parent
          (set-parent-link! g (uplink-of g replacement) parent)
          (clear-parent-link! g (uplink-of g replacement))))))

  (define (replace-kind! st g active-link term kind)
    (let* ([old-occurrences (push-direct-free-vars-of-term g term '())]
           [occurrence-set (make-entity-set)])
      (term-kind-set! (graph-term g term) kind)
      (let ([new-occurrences (push-direct-free-vars-of-term g term '())])
        (set-extend! occurrence-set new-occurrences)
        (for-each
          (lambda (occ)
            (unless (entity-set-contains? occurrence-set occ)
              (kill-occurrence! st g occ)))
          old-occurrences)
        (for-each (lambda (occ) (set-free-owner! g occ active-link)) new-occurrences))))

  (define (kill-occurrence! st g occ)
    (let ([binder (free-binder g occ)])
      (if (free-occ-is-singleton-verified? g occ)
        (begin
          (set-bound-var-occ! g binder #f)
          (worklist-queue-add! (optimizer-state-dead-bindings st) binder))
        (let ([next (free-occ-next g occ)])
          (free-occ-remove! g occ)
          (when (eqv? (bound-var-occ g binder) occ)
            (set-bound-var-occ! g binder next))))))

  (define (kill-binding! st g occ)
    (let ([binding (free-binder g occ)])
      (set-bound-var-occ! g binding #f)
      (worklist-queue-add! (optimizer-state-dead-bindings st) binding)
      (let ([link (st-known-function st binding)])
        (when link (clear-function-link! g link)))))

  (define (enqueue-known-occurrences! st g function args)
    (graph-worklist-add-known-occurrences! (optimizer-state-worklist st) g
      (free-vars-slice g args)
      (bound-vars-slice g (function-vars (fn g function)))
      (lambda (binder) (and (st-known-expr st binder) #t))))

  (define (collect-redexes-using-fn-ret-cont! st g oldret)
    (for-each-occurrence g oldret
      (lambda (occ)
        (let ([term (read-term-link g (free-owner g occ))])
          (when term
            (let ([kind (kind-of g term)])
              (when (k-app? kind)
                (let ([def (st-function-def st (free-binder g (k-app-func kind)))])
                  (when def (st-add-subterm! st def))))))))))

  (define (continuation-body-is-simple-inline? st g function)
    (let* ([data (fn g function)]
           [value-scope (make-entity-set)])
      (set-extend! value-scope (bound-vars-slice g (function-vars data)))
      (when (function-variadic data)
        (entity-set-insert! value-scope (function-variadic data)))
      (term-is-simple-continuation-inline-body? st g (function-body data) value-scope)))

  (define (term-is-simple-continuation-inline-body? st g link value-scope)
    (let ([term (read-term-link g link)])
      (and term
        (let ([kind (kind-of g term)])
          (cond
            [(k-let-val? kind)
              (let ([expr (read-expr-link g (k-let-val-expr kind))])
                (and (or (not expr)
                         (expr-free-occurrences-are-allowed? g expr value-scope))
                     (begin
                       (entity-set-insert! value-scope (k-let-val-binder kind))
                       (term-is-simple-continuation-inline-body? st g (k-let-val-body kind)
                         value-scope))))]
            [(k-if? kind)
              (let ([then-scope (set-copy value-scope)]
                    [else-scope (set-copy value-scope)])
                (and (term-is-simple-continuation-inline-body? st g (k-if-then kind) then-scope)
                     (term-is-simple-continuation-inline-body? st g (k-if-else kind) else-scope)))]
            [(k-continue? kind)
              (and (continuation-target-is-inline-stable? st g (k-continue-cont kind))
                   (free-occurrences-are-allowed? g (free-vars-slice g (k-continue-vars kind))
                     value-scope))]
            [(k-app? kind)
              (and (continuation-target-is-inline-stable? st g (k-app-cont kind))
                   (free-occurrences-are-allowed? g (free-vars-slice g (k-app-vars kind))
                     value-scope)
                   (entity-set-contains? value-scope (free-binder g (k-app-func kind))))]
            [(k-raise? kind)
              (free-occurrences-are-allowed? g (free-vars-slice g (k-raise-vars kind)) value-scope)]
            [(k-branch-prim? kind)
              (and (continuation-target-is-inline-stable? st g (k-branch-prim-consequent kind))
                   (continuation-target-is-inline-stable? st g (k-branch-prim-alternative kind))
                   (free-occurrences-are-allowed? g (free-vars-slice g (k-branch-prim-vars kind))
                     value-scope))]
            [else #f])))))

  (define (continuation-target-is-inline-stable? st g target)
    (let ([binder (free-binder g target)])
      (and (not (st-known-function st binder))
           (not (st-return-cont-owner st binder)))))

  (define (expr-free-occurrences-are-allowed? g expr allowed)
    (let ([all #t])
      (for-each-free-var-of-expr g expr
        (lambda (var)
          (set! all (and (entity-set-contains? allowed (free-binder g var)) all))))
      all))

  (define (free-occurrences-are-allowed? g vars allowed)
    (let loop ([vars vars])
      (or (null? vars)
          (and (entity-set-contains? allowed (free-binder g (car vars)))
               (loop (cdr vars))))))

  (define (try-contify-fix! st g active-link term functions body mode)
    (let ([live (live-function-links g functions)])
      (if (null? live)
        (replace-with-existing-body! st g active-link term body)
        (let* ([choose (lambda (g site return-cont binders fns)
                         (choose-contification-insertion st g site return-cont binders fns))]
               [owner-of (lambda (f) (st-function-owner-def st f))]
               [scc (lambda () (scc:find-candidate choose g active-link term live body owner-of))]
               [dom (lambda () (dom:find-candidate choose g active-link term live body owner-of))]
               [candidate
                 (case mode
                   [(Off) #f]
                   [(Scc) (scc)]
                   [(Dom) (dom)]
                   [else (or (dom) (scc))])])
          (when candidate
            (contify-candidate! st g active-link term live body candidate))))))

  (define (requeue-enclosing-fixes! st g from)
    (let ([start (read-term-link g from)])
      (when start
        (let loop ([term start] [n 0])
          (when (< n 4096)
            (let ([parent (read-parent-link g (uplink-of g term))])
              (when parent
                (if (parent-term? parent)
                  (let ([up (parent-id parent)])
                    (when (k-fix? (kind-of g up))
                      (let ([link (get-active-link-for g up)])
                        (when link (st-add-subterm! st link))))
                    (loop up (+ n 1)))
                  (let ([owner (st-function-owner-def st (parent-id parent))])
                    (when owner
                      (let ([up (read-term-link g owner)])
                        (when up
                          (when (k-fix? (kind-of g up))
                            (st-add-subterm! st owner))
                          (loop up (+ n 1))))))))))))))

  (define (contify-candidate! st g active-link term live body candidate)
    (let ([binders (scc:contify-candidate-binders candidate)]
          [return-cont (scc:contify-candidate-return-cont candidate)]
          [source (scc:contify-candidate-source candidate)])
      (let-values ([(contified-links contified-functions untouched-links)
                    (let loop ([l live] [cl '()] [cf '()] [ul '()])
                      (if (null? l)
                        (values (reverse cl) (reverse cf) (reverse ul))
                        (let ([link (caar l)] [function (cdar l)])
                          (if (entity-set-contains? binders (function-var (fn g function)))
                            (loop (cdr l) (cons link cl) (cons function cf) ul)
                            (loop (cdr l) cl cf (cons link ul))))))])
        (unless (null? contified-functions)
          (let ([insertion (scc:contify-candidate-insertion candidate)]
                [contified (new-function-links g contified-links)])
            (if (null? untouched-links)
              (begin
                (replace-with-existing-body! st g active-link term body)
                (when (eqv? (scc:contify-insertion-site insertion) body)
                  (set! insertion (scc:contify-insertion-with-site insertion active-link))))
              (let ([untouched (new-function-links g untouched-links)])
                (term-kind-set! (graph-term g term) (make-k-fix untouched body))
                (backpatch-subterms! g term (subterms-of g term))))
            (let ([site (scc:contify-insertion-site insertion)]
                  [n (length contified-functions)]
                  [stats (st-stats st)])
              (stat-inc! stats 'contifications)
              (stat-add! stats 'contified-functions n)
              (if (eq? source 'Scc)
                (begin (stat-inc! stats 'scc-contifications)
                       (stat-add! stats 'scc-contified-functions n))
                (begin (stat-inc! stats 'dom-contifications)
                       (stat-add! stats 'dom-contified-functions n)))
              (for-each
                (lambda (function)
                  (let ([old-ret (function-cont (fn g function))])
                    (when old-ret
                      (collect-redexes-using-fn-ret-cont! st g old-ret)
                      (subst-var-for-binders! g return-cont old-ret)
                      (let ([data (fn g function)])
                        (function-cont-set! data #f)
                        (function-is-rec-set! data #f)
                        (function-is-cold-set! data #f)
                        (function-is-noinline-set! data #f)
                        (function-is-reified-set! data #f))
                      (mset! (optimizer-state-return-cont-owners st) old-ret #f))))
                contified-functions)
              (insert-contified-functions! st g insertion contified)
              (for-each
                (lambda (function)
                  (let ([binder (function-var (fn g function))])
                    (mset! (optimizer-state-function-defs st) binder site)
                    (mset! (optimizer-state-function-owner-defs st) function site)
                    (backpatch-function! g function)))
                contified-functions)
              (transform-apps-to-continues! st g site binders)
              (collect-redexes! st g site)
              (st-add-subterm! st site)
              (requeue-enclosing-fixes! st g site)
              (unless (null? untouched-links)
                (st-add-subterm! st active-link))))))))

  (define (wrap-link-with-letk! st g link functions)
    (let* ([old-term (read-term-link g link)]
           [old-parent (read-parent-link g (uplink-of g old-term))]
           [body (new-term-link g old-term)]
           [wrapper-link (new-parent-link g old-parent)]
           [wrapper (new-term g wrapper-link (make-k-letk functions body)
                      (term-source (graph-term g old-term)))])
      (set-term-link! g link wrapper)
      (set-parent-link! g (uplink-of g old-term) (parent-term wrapper))
      wrapper))

  (define (insert-contified-functions! st g insertion functions)
    (if (eq? (scc:contify-insertion-kind insertion) 'Wrap)
      (wrap-link-with-letk! st g (scc:contify-insertion-site insertion) functions)
      (extend-letk-with-functions! st g (scc:contify-insertion-site insertion) functions)))

  (define (extend-letk-with-functions! st g link functions)
    (let* ([term (read-term-link g link)]
           [kind (kind-of g term)]
           [links (append (function-links-slice g (k-letk-functions kind))
                    (function-links-slice g functions))])
      (term-kind-set! (graph-term g term)
        (make-k-letk (new-function-links g links) (k-letk-body kind)))
      term))

  (define (transform-apps-to-continues! st g link binders)
    (let ([term (read-term-link g link)])
      (when term
        (let ([kind (kind-of g term)])
          (cond
            [(k-let-val? kind)
              (transform-apps-to-continues! st g (k-let-val-body kind) binders)]
            [(group-kind? kind)
              (let ([bodies
                     (let loop ([links (function-links-slice g (kind-functions kind))] [acc '()])
                       (if (null? links)
                         (reverse acc)
                         (let ([function (read-function-link g (car links))])
                           (loop (cdr links)
                             (if function (cons (function-body (fn g function)) acc) acc)))))])
                (for-each (lambda (b) (transform-apps-to-continues! st g b binders)) bodies)
                (transform-apps-to-continues! st g (kind-fix-body kind) binders))]
            [(k-if? kind)
              (transform-apps-to-continues! st g (k-if-then kind) binders)
              (transform-apps-to-continues! st g (k-if-else kind) binders)]
            [(and (k-app? kind)
                  (entity-set-contains? binders (free-binder g (k-app-func kind))))
              (replace-kind! st g link term
                (make-k-continue (k-app-func kind) (k-app-vars kind)))
              (st-add-subterm! st link)]
            [else (if #f #f)])))))

  ;; => insertion or #f
  (define (choose-contification-insertion st g site return-cont binders functions)
    (let ([site-term (read-term-link g site)])
      (and site-term
        (cond
          [(contification-bindings-available-at? st g site-term return-cont binders functions 'Wrapper)
            (scc:make-contify-insertion 'Wrap site)]
          [(and (k-letk? (kind-of g site-term))
                (contification-bindings-available-at? st g site-term return-cont binders functions 'AtTerm))
            (scc:make-contify-insertion 'ExtendLetk site)]
          [else #f]))))

  (define (contification-bindings-available-at? st g site-term return-cont binders functions availability)
    (and (binder-is-available-for-contification? st g site-term return-cont availability)
         (let loop ([fs functions])
           (or (null? fs)
               (and (function-uses-only-available-scope? st g (car fs) binders site-term availability)
                    (loop (cdr fs)))))))

  (define (binder-is-available-for-contification? st g term binder availability)
    (if (eq? availability 'AtTerm)
      (binder-is-available-at-term? st g term binder)
      (binder-is-available-at-contification-wrapper? st g term binder)))

  ;; Shared body of the two availability predicates.  WRAPPER? selects the
  ;; stricter function-def rule.
  (define (binder-available? st g term binder wrapper?)
    (let ([owner (st-return-cont-owner st binder)])
      (cond
        [owner (term-is-inside-function? st g term owner)]
        [(st-known-expr st binder)
          => (lambda (expr-link)
               (let ([expr (read-expr-link g expr-link)])
                 (and expr
                   (let ([parent (read-parent-link g (expr-uplink (graph-expr g expr)))])
                     (and parent (parent-term? parent)
                       (let* ([def-term (parent-id parent)]
                              [kind (kind-of g def-term)])
                         (and (k-let-val? kind)
                              (eqv? (k-let-val-binder kind) binder)
                              (let ([body-term (read-term-link g (k-let-val-body kind))])
                                (and body-term
                                     (term-is-inside-term-scope? st g term body-term))))))))))]
        [(st-function-def st binder)
          => (lambda (def)
               (let ([scope-term (read-term-link g def)])
                 (and scope-term
                   (if wrapper?
                     (and (not (eqv? term scope-term))
                          (term-is-inside-term-scope? st g term scope-term))
                     (term-is-inside-term-scope? st g term scope-term)))))]
        [else #t])))

  (define (binder-is-available-at-term? st g term binder)
    (binder-available? st g term binder #f))

  (define (binder-is-available-at-contification-wrapper? st g term binder)
    (binder-available? st g term binder #t))

  (define (function-uses-only-available-scope? st g function group-binders site-term availability)
    (let* ([data (fn g function)]
           [local (set-copy group-binders)])
      (set-extend! local (bound-vars-slice g (function-vars data)))
      (when (function-cont data) (entity-set-insert! local (function-cont data)))
      (when (function-variadic data) (entity-set-insert! local (function-variadic data)))
      (term-uses-only-available-scope-at-term? st g (function-body data) local site-term availability)))

  (define (term-uses-only-available-scope-at-term? st g link local site-term availability)
    (let ([term (read-term-link g link)])
      (or (not term)
        (let ([kind (kind-of g term)])
          (cond
            [(k-let-val? kind)
              (let ([expr (read-expr-link g (k-let-val-expr kind))])
                (and (or (not expr)
                         (expr-free-occurrences-available? st g expr local site-term availability))
                     (let ([body-local (set-copy local)])
                       (entity-set-insert! body-local (k-let-val-binder kind))
                       (term-uses-only-available-scope-at-term? st g (k-let-val-body kind)
                         body-local site-term availability))))]
            [(group-kind? kind)
              (let* ([nested
                      (let loop ([links (function-links-slice g (kind-functions kind))] [acc '()])
                        (if (null? links)
                          (reverse acc)
                          (let ([function (read-function-link g (car links))])
                            (loop (cdr links) (if function (cons function acc) acc)))))]
                     [body-local (set-copy local)])
                (for-each (lambda (f) (entity-set-insert! body-local (function-var (fn g f)))) nested)
                (and (let loop ([fs nested])
                       (or (null? fs)
                           (let* ([data (fn g (car fs))]
                                  [function-local (set-copy body-local)])
                             (set-extend! function-local (bound-vars-slice g (function-vars data)))
                             (when (function-cont data)
                               (entity-set-insert! function-local (function-cont data)))
                             (when (function-variadic data)
                               (entity-set-insert! function-local (function-variadic data)))
                             (and (term-uses-only-available-scope-at-term? st g (function-body data)
                                    function-local site-term availability)
                                  (loop (cdr fs))))))
                     (term-uses-only-available-scope-at-term? st g (kind-fix-body kind)
                       body-local site-term availability)))]
            [(k-if? kind)
              (and (free-occurrences-available? st g (list (k-if-test kind)) local site-term availability)
                   (term-uses-only-available-scope-at-term? st g (k-if-then kind) local site-term availability)
                   (term-uses-only-available-scope-at-term? st g (k-if-else kind) local site-term availability))]
            [else
              (direct-free-occurrences-available? st g term local site-term availability)])))))

  (define (var-available? st g var local site-term availability)
    (let ([binder (free-binder g var)])
      (or (entity-set-contains? local binder)
          (binder-is-available-for-contification? st g site-term binder availability))))

  ;; Rust folds with `&=' (no short circuit); the result is the same.
  (define (expr-free-occurrences-available? st g expr local site-term availability)
    (let ([available #t])
      (for-each-free-var-of-expr g expr
        (lambda (var)
          (set! available (and (var-available? st g var local site-term availability) available))))
      available))

  (define (direct-free-occurrences-available? st g term local site-term availability)
    (let ([available #t])
      (for-each-direct-free-var-of-term g term
        (lambda (var)
          (set! available (and (var-available? st g var local site-term availability) available))))
      available))

  (define (free-occurrences-available? st g vars local site-term availability)
    (let loop ([vars vars])
      (or (null? vars)
          (and (var-available? st g (car vars) local site-term availability)
               (loop (cdr vars))))))

  (define (term-is-inside-function? st g term owner)
    (let loop ([term term])
      (let ([parent (read-parent-link g (uplink-of g term))])
        (and parent
          (if (parent-term? parent)
            (loop (parent-id parent))
            (let ([function (parent-id parent)])
              (or (eqv? function owner)
                  (let ([definition (st-function-owner-def st function)])
                    (and definition
                      (let ([def-term (read-term-link g definition)])
                        (and def-term (loop def-term))))))))))))

  (define (term-is-inside-any-function? g term)
    (let loop ([term term])
      (let ([parent (read-parent-link g (uplink-of g term))])
        (and parent
          (or (parent-func? parent)
              (loop (parent-id parent)))))))

  (define (term-is-inside-term-scope? st g term scope)
    (or (eqv? term scope)
      (let loop ([term term])
        (let ([parent (read-parent-link g (uplink-of g term))])
          (and parent
            (if (parent-term? parent)
              (or (eqv? (parent-id parent) scope)
                  (loop (parent-id parent)))
              (let ([def (st-function-owner-def st (parent-id parent))])
                (and def
                  (let ([def-term (read-term-link g def)])
                    (and def-term
                      (or (eqv? def-term scope)
                          (loop def-term))))))))))))

)

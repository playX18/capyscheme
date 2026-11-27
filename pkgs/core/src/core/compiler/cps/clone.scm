;; Subterm cloning for graphical CPS.
(library (core compiler cps clone)
  (export
    make-graph-clone make-graph-clone/substitutions graph-clone?
    graph-clone-subterm! graph-clone-subterm-into!)
  (import (core) (capy) (core compiler cps graph))

  (define-record-type (graph-clone %make-graph-clone graph-clone?)
    (fields graph (immutable binders clone-binders)))

  ;; The binders map: old binder -> new binder. The ids are the graph's, up to its
  ;; size, while a clone maps the few binders of the subterm, so a table (a vector
  ;; indexed by id cost the size of the graph for every inlined call).
  (define (make-graph-clone graph)
    (%make-graph-clone graph (make-eqv-hashtable)))

  (define (make-graph-clone/substitutions graph substitutions)
    (let ([c (make-graph-clone graph)])
      (for-each (lambda (pair) (binder-set! c (car pair) (cdr pair))) substitutions)
      c))

  (define (binder-set! c old new)
    (hashtable-set! (clone-binders c) old new))

  (define (mapped-binder c binder)
    (hashtable-ref (clone-binders c) binder binder))

  (define (fresh-binder c binder)
    (let* ([g (graph-clone-graph c)]
           [fresh (new-bound-var g (lvar-copy (bound-var-data-var (graph-bound-var g binder))))])
      (binder-set! c binder fresh)
      fresh))

  (define (clone-free-occurrence c occ owner)
    (let* ([g (graph-clone-graph c)]
           [binder (mapped-binder c (free-binder g occ))])
      (new-free-occ-for-binder g binder owner)))

  (define (clone-free-vars c vars owner)
    (let ([g (graph-clone-graph c)])
      (new-free-vars g
        (let loop ([vars (free-vars-slice g vars)] [acc '()])
          (if (null? vars)
            (reverse acc)
            (loop (cdr vars) (cons (clone-free-occurrence c (car vars) owner) acc)))))))

  (define (clone-subterm! c root)
    (let* ([g (graph-clone-graph c)]
           [clone (new-term-link g #f)])
      (and (clone-subterm-into! c root clone) clone)))

  (define (clone-subterm-into! c source target)
    (let* ([g (graph-clone-graph c)]
           [term (read-term-link g source)])
      (and term (clone-term-into c term target))))

  (define (graph-clone-subterm! c root) (clone-subterm! c root))
  (define (graph-clone-subterm-into! c source target) (clone-subterm-into! c source target))

  (define (clone-subterm-link c source)
    (let* ([g (graph-clone-graph c)]
           [clone (new-term-link g #f)])
      (and (clone-subterm-into! c source clone) clone)))

  (define (clone-expr-link c source owner)
    (let* ([g (graph-clone-graph c)]
           [expr (read-expr-link g source)])
      (and expr
        (let* ([data (graph-expr g expr)]
               [kind (expr-kind data)]
               [new-kind
                 (if (k-literal? kind)
                   (make-k-literal (k-literal-value kind))
                   (make-k-prim-call (k-prim-call-prim kind)
                     (clone-free-vars c (k-prim-call-vars kind) owner)))]
               [link (new-expr-link g #f)]
               [uplink (new-parent-link g #f)]
               [clone (new-expr g uplink new-kind (expr-source data))])
          (set-expr-link! g link clone)
          link))))

  ;; Returns the new term id, or #f when a dead link is hit.
  (define (clone-term-into c source target)
    (let* ([g (graph-clone-graph c)]
           [data (graph-term g source)]
           [uplink (new-parent-link g #f)]
           [kind (term-kind data)]
           [new-kind
             (cond
               [(k-let-val? kind)
                 (let* ([binder (fresh-binder c (k-let-val-binder kind))]
                        [expr (clone-expr-link c (k-let-val-expr kind) target)])
                   (and expr
                     (let ([body (clone-subterm-link c (k-let-val-body kind))])
                       (and body (make-k-let-val binder expr body)))))]
               [(k-fix? kind)
                 (let ([functions (clone-function-links c (k-fix-functions kind))])
                   (and functions
                     (let ([body (clone-subterm-link c (k-fix-body kind))])
                       (and body (make-k-fix functions body)))))]
               [(k-letk? kind)
                 (let ([functions (clone-function-links c (k-letk-functions kind))])
                   (and functions
                     (let ([body (clone-subterm-link c (k-letk-body kind))])
                       (and body (make-k-letk functions body)))))]
               [(k-if? kind)
                 (let* ([test (clone-free-occurrence c (k-if-test kind) target)]
                        [consequent (clone-subterm-link c (k-if-then kind))])
                   (and consequent
                     (let ([alternative (clone-subterm-link c (k-if-else kind))])
                       (and alternative
                         (make-k-if test consequent alternative (k-if-hints kind))))))]
               [(k-continue? kind)
                 (let* ([cont (clone-free-occurrence c (k-continue-cont kind) target)]
                        [vars (clone-free-vars c (k-continue-vars kind) target)])
                   (make-k-continue cont vars))]
               [(k-app? kind)
                 (let* ([func (clone-free-occurrence c (k-app-func kind) target)]
                        [vars (clone-free-vars c (k-app-vars kind) target)]
                        [cont (clone-free-occurrence c (k-app-cont kind) target)])
                   (make-k-app func vars cont))]
               [(k-raise? kind)
                 (let ([vars (clone-free-vars c (k-raise-vars kind) target)])
                   (make-k-raise (k-raise-kind kind) vars))]
               [(k-branch-prim? kind)
                 (let* ([vars (clone-free-vars c (k-branch-prim-vars kind) target)]
                        [consequent (clone-free-occurrence c (k-branch-prim-consequent kind) target)]
                        [alternative (clone-free-occurrence c (k-branch-prim-alternative kind) target)])
                   (make-k-branch-prim (k-branch-prim-op kind) consequent alternative vars))]
               [else (assertion-violation 'clone-term-into "bad term kind" kind)])])
      (and new-kind
        (let ([clone (new-term g uplink new-kind (term-source data))])
          (set-term-link! g target clone)
          (backpatch-term-children c clone target)
          clone))))

  ;; Returns the new function-links list, or #f.
  (define (clone-function-links c functions)
    (let* ([g (graph-clone-graph c)]
           [source-links (function-links-slice g functions)])
      (let loop ([links source-links] [clones '()] [cloned-links '()])
        (if (pair? links)
          (let ([source (read-function-link g (car links))])
            (if (not source)
              (loop (cdr links) clones cloned-links)
              (let* ([data (graph-function g source)]
                     [var (fresh-binder c (function-var data))]
                     [vars (new-bound-vars g
                             (let vloop ([vs (bound-vars-slice g (function-vars data))] [acc '()])
                               (if (null? vs)
                                 (reverse acc)
                                 (vloop (cdr vs) (cons (fresh-binder c (car vs)) acc)))))]
                     [variadic (and (function-variadic data)
                                 (fresh-binder c (function-variadic data)))]
                     [cont (and (function-cont data) (fresh-binder c (function-cont data)))]
                     [body (new-term-link g #f)]
                     [clone (new-function g
                              (make-function
                                (function-name data) (function-source data) var vars
                                variadic cont (function-is-variadic data) body
                                (function-is-rec data) (function-is-cold data)
                                (function-is-noinline data) (function-is-reified data)
                                (function-meta data) (function-case-group data)))]
                     [clone-link (new-function-link g clone)])
                (set-function-link! g clone-link clone)
                (loop (cdr links)
                  (cons (cons source clone) clones)
                  (cons clone-link cloned-links)))))
          ;; second phase: clone the bodies, in source order
          (let body-loop ([pairs (reverse clones)])
            (if (null? pairs)
              (new-function-links g (reverse cloned-links))
              (let ([source (caar pairs)] [clone (cdar pairs)])
                (if (clone-subterm-into! c
                      (function-body (graph-function g source))
                      (function-body (graph-function g clone)))
                  (begin
                    (backpatch-function! g clone)
                    (body-loop (cdr pairs)))
                  #f))))))))

  (define (backpatch-term-children c term owner)
    (let* ([g (graph-clone-graph c)]
           [kind (term-kind (graph-term g term))])
      (cond
        [(k-let-val? kind)
          (backpatch-subexprs! g term owner (list (k-let-val-expr kind)))
          (backpatch-subterms! g term (list (k-let-val-body kind)))]
        [(k-fix? kind) (backpatch-subterms! g term (list (k-fix-body kind)))]
        [(k-letk? kind) (backpatch-subterms! g term (list (k-letk-body kind)))]
        [(k-if? kind)
          (backpatch-subterms! g term (list (k-if-then kind) (k-if-else kind)))]
        [else (if #f #f)]))))

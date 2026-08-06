;; TreeIL traversal combinators.
;;
;; This library provides the generic walkers used by the compiler passes
;; that massage TreeIL terms:
;;
;;   * `make-tree-il-folder' builds a closure that visits every sub-term
;;     of a tree in pre-order, threading one or more seed values through
;;     a `down' callback (called on entry to a node) and an `up' callback
;;     (called once the node's children have been visited);
;;
;;   * `pre-post-order' / `pre-order' / `post-order' rebuild a term from
;;     the bottom up.  Each node is first rewritten by the `pre'
;;     callback, its children are then visited and stitched back together
;;     with the record constructors, and the result is finally handed to
;;     the `post' callback.  When no child changed, the original node
;;     object is returned unchanged, so callers can detect a no-op pass
;;     with an eq?-test.

(library (capy compiler tree-il fold)
  (export
    pre-order
    post-order
    pre-post-order
    make-tree-il-folder
    tree-il-fold)
  (import (capy compiler tree-il terms)
    (srfi 257)
    (rnrs))

  ;; make-tree-il-folder: expand into a folder closure with the given
  ;; seed names.  The closure is invoked as
  ;;
  ;;   (folder tree down up seed ...)
  ;;
  ;; where `down' is called as (down node seed ...) before the node's
  ;; children are visited and `up' as (up node seed ...) afterwards; the
  ;; values returned by `up' are the result of the fold.  Children are
  ;; visited in evaluation order: operator before operands, binding
  ;; values before the body, test before branches, and so on.
  (define-syntax make-tree-il-folder
    (syntax-rules ()
      [(_ seed ...)
        (lambda (tree down up seed ...)
          ;; Thread the seeds through a list of child terms.
          (define (sweep-children visit exps seed ...)
            (if (null? exps)
              (values seed ...)
              (let-values ([(seed ...) (visit (car exps) seed ...)])
                (sweep-children visit (cdr exps) seed ...))))

          (let recur ([node tree] [seed seed] ...)
            (let*-values
              (([seed ...] (down node seed ...))
                ([seed ...]
                  (match node
                    ;; single-expression nodes
                    [(~or
                        (~lset _ _ _ exp)
                        (~module-set _ _ _ _ exp)
                        (~toplevel-set _ _ _ exp)
                        (~toplevel-define _ _ _ exp))
                      (recur exp seed ...)]
                    ;; binding forms: values first, then the body
                    [(~or
                        (~let _ _ _ _ init body)
                        (~fix _ _ _ init body))
                      (let*-values (([seed ...] (sweep-children recur init seed ...)))
                        (recur body seed ...))]
                    [(~receive _ _ _ producer consumer)
                      (let*-values (([seed ...] (recur producer seed ...)))
                        (recur consumer seed ...))]
                    [(~application _ operator operands)
                      (let*-values (([seed ...] (recur operator seed ...)))
                        (sweep-children recur operands seed ...))]
                    [(~primcall _ _ args)
                      (sweep-children recur args seed ...)]
                    [(~proc _ _ body _ _)
                      (recur body seed ...)]
                    [(~values _ vals)
                      (sweep-children recur vals seed ...)]
                    [(~sequence _ head tail)
                      (let*-values (([seed ...] (recur head seed ...)))
                        (recur tail seed ...))]
                    [(~wcm _ _ mark result)
                      (let*-values (([seed ...] (recur mark seed ...)))
                        (recur result seed ...))]
                    [(~if _ test then els)
                      (let*-values (([seed ...] (recur test seed ...))
                                    ([seed ...] (recur then seed ...)))
                        (recur els seed ...))]
                    [_ (values seed ...)])))
              (up node seed ...))))]))

  (define (tree-il-fold down up seed tree)
    ;; Traverse TREE, calling DOWN on the way in and UP on the way out of
    ;; each sub-term, threading SEED through both as extra values.  Returns
    ;; the values produced by the outermost UP call.
    ((make-tree-il-folder seed) tree down up seed))

  (define (pre-post-order pre post term)
    ;; Are two child lists elementwise eq??  Used to detect whether any
    ;; child was rebuilt during the walk.
    (define (same-contents? a b)
      (or (null? a)
        (and (eq? (car a) (car b))
          (same-contents? (cdr a) (cdr b)))))

    (let walk ([node term])
      (post
        (let ([node (pre node)])
          (cond
            ;; leaves: nothing to rebuild
            [(or (void? node)
                (constant? node)
                (lref? node)
                (primref? node)
                (module-ref? node)
                (toplevel-ref? node))
              node]
            ;; single-expression nodes
            [(lset? node)
              (let ([value* (walk (lset-value node))])
                (if (not (eq? value* (lset-value node)))
                  (make-lset (term-src node) (lset-name node) (lset-sym node) value*)
                  node))]
            [(module-set? node)
              (let ([value* (walk (module-set-value node))])
                (if (not (eq? value* (module-set-value node)))
                  (make-module-set (term-src node) (module-set-module node)
                    (module-set-name node)
                    (module-set-public? node)
                    value*)
                  node))]
            [(toplevel-set? node)
              (let ([value* (walk (toplevel-set-value node))])
                (if (not (eq? value* (toplevel-set-value node)))
                  (make-toplevel-set (term-src node)
                    (toplevel-set-mod node)
                    (toplevel-set-name node)
                    value*)
                  node))]
            [(toplevel-define? node)
              (let ([value* (walk (toplevel-define-value node))])
                (if (not (eq? value* (toplevel-define-value node)))
                  (make-toplevel-define (term-src node)
                    (toplevel-define-mod node)
                    (toplevel-define-name node)
                    value*)
                  node))]
            ;; conditionals
            [(if? node)
              (let* ([test* (walk (if-test node))]
                    [then* (walk (if-then node))]
                    [else* (walk (if-else node))])
                (if (not (and (eq? test* (if-test node))
                         (eq? then* (if-then node))
                         (eq? else* (if-else node))))
                  (make-if (term-src node) test* then* else*)
                  node))]
            ;; binding forms
            [(let? node)
              (unless (list? (let-rhs node))
                (assertion-violation 'fold "malformed let" (let-rhs node) (let-ids node) (let-lhs node) (term-src node)))
              (let* ([rhs* (map walk (let-rhs node))]
                    [body* (walk (let-body node))])
                (if (not (and (same-contents? rhs* (let-rhs node))
                         (eq? body* (let-body node))))
                  (make-let (term-src node)
                    (let-style node)
                    (let-ids node)
                    (let-lhs node)
                    rhs*
                    body*)
                  node))]
            [(fix? node)
              (unless (list? (fix-rhs node))
                (assertion-violation 'fold "malformed fix" (fix-rhs node) (term-src node)))
              (let* ([rhs* (map walk (fix-rhs node))]
                    [body* (walk (fix-body node))])
                (if (not (and (same-contents? rhs* (fix-rhs node))
                         (eq? body* (fix-body node))))
                  (make-fix (term-src node)
                    (fix-ids node)
                    (fix-lhs node)
                    rhs*
                    body*)
                  node))]
            [(receive? node)
              (let* ([producer* (walk (receive-producer node))]
                    [consumer* (walk (receive-consumer node))])
                (if (not (and (eq? producer* (receive-producer node))
                         (eq? consumer* (receive-consumer node))))
                  (make-receive (term-src node)
                    (receive-ids node)
                    (receive-vars node)
                    producer*
                    consumer*)
                  node))]
            ;; calls
            [(application? node)
              (let ([operator* (walk (application-operator node))])
                (unless (list? (application-operands node))
                  (assertion-violation 'fold "malformed application" (application-operands node) (term-src node)))
                (let* ([operands* (map walk (application-operands node))])
                  (if (not (and (eq? operator* (application-operator node))
                           (same-contents? operands* (application-operands node))))
                    (make-application (term-src node)
                      operator*
                      operands*)
                    node)))]
            [(primcall? node)
              (unless (list? (primcall-args node))
                (assertion-violation 'fold "malformed primcall" (primcall-prim node) (primcall-args node) (term-src node)))
              (let ([args* (map walk (primcall-args node))])
                (if (not (same-contents? args* (primcall-args node)))
                  (make-primcall (term-src node)
                    (primcall-prim node)
                    args*)
                  node))]
            ;; procedures
            [(proc? node)
              (let ([body* (walk (proc-body node))])
                (if (not (eq? body* (proc-body node)))
                  (make-proc (term-src node)
                    (proc-args node)
                    body*
                    (proc-meta node)
                    (proc-ids node))
                  node))]
            ;; multiple values
            [(values? node)
              (unless (list? (values-values node))
                (assertion-violation 'fold "malformed values" (values-values node) (term-src node)))
              (let ([vals* (map walk (values-values node))])
                (if (not (same-contents? vals* (values-values node)))
                  (make-values (term-src node)
                    vals*)
                  node))]
            ;; sequencing
            [(sequence? node)
              (let* ([head* (walk (sequence-head node))]
                    [tail* (walk (sequence-tail node))])
                (if (not (and (eq? head* (sequence-head node))
                         (eq? tail* (sequence-tail node))))
                  (make-sequence (term-src node)
                    head*
                    tail*)
                  node))]
            ;; continuation marks
            [(wcm? node)
              (let* ([mark* (walk (wcm-mark node))]
                    [result* (walk (wcm-result node))])
                (if (not (and (eq? mark* (wcm-mark node))
                         (eq? result* (wcm-result node))))
                  (make-wcm (term-src node)
                    (wcm-key node)
                    mark*
                    result*)
                  node))]
            [else (error 'pre-post-order "unknown TreeIL term" node)])))))

  (define (post-order f x)
    (pre-post-order (lambda (x) x) f x))
  (define (pre-order f x)
    (pre-post-order f (lambda (x) x) x)))
;; Merge nested non-interleaved `Fix'/`Letk' groups.
;;
;; Port of crates/capy/src/compiler/cps/fixmerge.rs.
;;
;; Environment sharing is per-site, so lambdas and reified continuations bound
;; at the same point must be one group.  The TreeIL fixing-letrec passes emit a
;; separate single-member Fix/Letk node per lambda (Fix(f, Fix(g, Fix(h, body))))
;; and this pass merges such chains into one Fix group.  Non-reified
;; continuations stay in a trailing Letk.  Runs after `reify-graph!'.
;;
;; API:
;;   (merge-nested-groups! graph root)   ROOT is a Subterm (term link), e.g.
;;                                       (function-body (graph-function g entry))
;;                                       / (program root link).  Rust:
;;                                       merge_nested_groups(&mut graph, root).
;; The Rust debug logging (dump_groups) is not ported.
(library (core compiler cps fixmerge)
  (export merge-nested-groups!)
  (import (core) (capy) (core compiler cps graph))

  (define (merge-nested-groups! g root)
    (let ([visited (make-eqv-hashtable)])
      (walk-term g root visited)))

  (define (walk-term g link visited)
    (let ([term (read-term-link g link)])
      (when (and term (not (hashtable-contains? visited term)))
        (hashtable-set! visited term #t)
        (let ([kind (term-kind (graph-term g term))])
          (cond
            [(k-fix? kind) (merge-chain g term (k-fix-functions kind) (k-fix-body kind) visited)]
            [(k-letk? kind) (merge-chain g term (k-letk-functions kind) (k-letk-body kind) visited)]
            [(k-let-val? kind) (walk-term g (k-let-val-body kind) visited)]
            [(k-if? kind)
              (walk-term g (k-if-then kind) visited)
              (walk-term g (k-if-else kind) visited)]
            [else (if #f #f)])))))

  (define (merge-chain g term functions body visited)
    (let ([group '()] [keep '()] [bodies '()])   ; all kept reversed
      ;; Partition one group's member links: live functions and reified
      ;; continuations join the merged group, live non-reified continuations
      ;; are kept for the trailing Letk.  Dead links stay with the group.
      (define (absorb-members! links)
        (for-each
          (lambda (link)
            (let ([function (read-function-link g link)])
              (if (not function)
                (set! group (cons link group))
                (let ([data (graph-function g function)])
                  (if (and (not (function-cont data)) (not (function-is-reified data)))
                    (set! keep (cons link keep))
                    (set! group (cons link group)))
                  (set! bodies (cons (function-body data) bodies))))))
          (function-links-slice g links)))
      (absorb-members! functions)
      (let loop ([chain-body body] [trailing #f])
        (let* ([body-term (read-term-link g chain-body)]
               [bkind (and body-term (term-kind (graph-term g body-term)))])
          (if (and bkind (or (k-fix? bkind) (k-letk? bkind)))
            (begin
              (absorb-members! (if (k-fix? bkind) (k-fix-functions bkind) (k-letk-functions bkind)))
              (loop (if (k-fix? bkind) (k-fix-body bkind) (k-letk-body bkind))
                (or trailing body-term)))
            (finish g term body chain-body trailing
              (reverse group) (reverse keep) (reverse bodies) visited))))))

  (define (finish g term body chain-body trailing group keep bodies visited)
    (cond
      ;; Only non-reified continuations (or dead links): leave untouched.
      [(null? group)
        (for-each (lambda (b) (walk-term g b visited)) bodies)
        (walk-term g body visited)]
      [else
        (let ([group-links (new-function-links g group)])
          (if (null? keep)
            (term-kind-set! (graph-term g term) (make-k-fix group-links chain-body))
            (let* ([keep-links (new-function-links g keep)]
                   [trailing-kind (make-k-letk keep-links chain-body)])
              (if trailing
                (begin
                  (term-kind-set! (graph-term g trailing) trailing-kind)
                  (term-kind-set! (graph-term g term) (make-k-fix group-links body)))
                (let* ([parent (new-parent-link g #f)]
                       [node (new-term g parent trailing-kind (term-source (graph-term g term)))])
                  (set-parent-link! g parent (parent-term term))
                  (let ([trailing-link (new-term-link g node)])
                    (term-kind-set! (graph-term g term) (make-k-fix group-links trailing-link))))))))
        (for-each (lambda (b) (walk-term g b visited)) bodies)
        (walk-term g chain-body visited)]))
)

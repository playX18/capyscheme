;; CFG graph utilities.
(library (core compiler cfg graph)
  (export loop-blocks block-successors)
  (import (core) (capy) (core compiler cfg ir))

  ;; The successors of `block' within `procedure': terminator successors plus
  ;; the entry block when the block tail-calls the procedure itself.
  (define (block-successors procedure block)
    (let ([term (block-terminator block)])
      (if (and (term-tail-call? term)
               (operand-local? (term-tail-call-callee term))
               (eqv? (operand-local-uvar (term-tail-call-callee term)) (cfg-proc-binding procedure)))
        (append (term-successors term) (list (cfg-proc-entry procedure)))
        (term-successors term))))

  (define (loop-blocks procedure)
    (let ([successors (make-eqv-hashtable)]
          [predecessors (make-eqv-hashtable)]
          [seen (make-eqv-hashtable)]
          [order '()]
          [loop-nodes (make-eqv-hashtable)])
      (define blocks (cfg-proc-blocks procedure))
      (for-each
        (lambda (b) (hashtable-set! successors (block-id b) (block-successors procedure b)))
        blocks)

      (define (dfs-postorder b)
        (unless (hashtable-contains? seen b)
          (hashtable-set! seen b #t)
          (for-each dfs-postorder (hashtable-ref successors b '()))
          (set! order (cons b order))))   ; `order' is reversed post-order

      (define (dfs-collect b component)
        (if (hashtable-contains? seen b)
          component
          (begin
            (hashtable-set! seen b #t)
            (let loop ([ps (reverse (hashtable-ref predecessors b '()))] [c (cons b component)])
              (if (null? ps)
                c
                (loop (cdr ps) (dfs-collect (car ps) c)))))))

      (for-each dfs-postorder (map block-id blocks))
      (for-each
        (lambda (b)
          (for-each
            (lambda (target)
              (hashtable-set! predecessors target
                (cons (block-id b) (hashtable-ref predecessors target '()))))
            (hashtable-ref successors (block-id b) '())))
        blocks)

      (hashtable-clear! seen)
      ;; `order' is already last-finished-first, i.e. `order.iter().rev()'.
      (for-each
        (lambda (b)
          (unless (hashtable-contains? seen b)
            (let ([component (dfs-collect b '())])
              (when (or (pair? (cdr component))
                        (memv b (hashtable-ref successors b '())))
                (for-each (lambda (n) (hashtable-set! loop-nodes n #t)) component)))))
        order)
      loop-nodes))
)

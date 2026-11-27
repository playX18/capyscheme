;; Worklist queues for graphical CPS optimization.
(library (core compiler cps worklist)
  (export
    make-worklist-queue worklist-queue? worklist-queue-empty? worklist-queue-length
    worklist-queue-add! worklist-queue-add-list! worklist-queue-get!
    worklist-queue-none worklist-queue-clear! worklist-queue->list
    make-graph-worklist graph-worklist? graph-worklist-pending-subterm-len
    graph-worklist-dead-binding-len graph-worklist-pending-fn-binder-len
    graph-worklist-empty? graph-worklist-add-subterm! graph-worklist-add-subterms!
    graph-worklist-get-subterm! graph-worklist-reconsider-function!
    graph-worklist-get-function-binder! graph-worklist-add-dead-binding!
    graph-worklist-get-dead-binding! graph-worklist-add-occurrences!
    graph-worklist-add-known-occurrences! graph-worklist-get-next-live-subterm!)
  (import (core) (capy) (core compiler cps graph))

  ;; --- FIFO queue: a front list and a reversed rear list ---------------

  (define worklist-queue-none (list 'none))

  (define-record-type (worklist-queue %make-worklist-queue worklist-queue?)
    (fields
      (mutable front queue-front queue-front-set!)
      (mutable rear queue-rear queue-rear-set!)
      (mutable len worklist-queue-length queue-len-set!)))

  (define (make-worklist-queue) (%make-worklist-queue '() '() 0))

  (define (worklist-queue-empty? q) (= 0 (worklist-queue-length q)))

  (define (worklist-queue-add! q item)
    (queue-rear-set! q (cons item (queue-rear q)))
    (queue-len-set! q (+ 1 (worklist-queue-length q))))

  (define (worklist-queue-add-list! q items)
    (for-each (lambda (item) (worklist-queue-add! q item)) items))

  (define (worklist-queue-get! q)
    (when (null? (queue-front q))
      (queue-front-set! q (reverse (queue-rear q)))
      (queue-rear-set! q '()))
    (if (null? (queue-front q))
      worklist-queue-none
      (let ([item (car (queue-front q))])
        (queue-front-set! q (cdr (queue-front q)))
        (queue-len-set! q (- (worklist-queue-length q) 1))
        item)))

  (define (worklist-queue-clear! q)
    (queue-front-set! q '())
    (queue-rear-set! q '())
    (queue-len-set! q 0))

  (define (worklist-queue->list q)
    (append (queue-front q) (reverse (queue-rear q))))

  ;; --- ordered id set with pop-min (BTreeSet<BoundVar>) -------------------
  ;; A binary min-heap plus a membership set; ids are inserted at most once
  ;; until popped.

  (define-record-type (min-set %make-min-set min-set?)
    (fields
      (mutable heap min-set-heap min-set-heap-set!)
      (mutable size min-set-size min-set-size-set!)
      members))

  (define (make-min-set) (%make-min-set (make-vector 16 0) 0 (make-entity-set)))

  (define (min-set-insert! s id)
    (when (entity-set-insert! (min-set-members s) id)
      (let ([n (min-set-size s)])
        (when (= n (vector-length (min-set-heap s)))
          (let ([bigger (make-vector (* 2 n) 0)])
            (let copy ([i 0])
              (when (< i n)
                (vector-set! bigger i (vector-ref (min-set-heap s) i))
                (copy (+ i 1))))
            (min-set-heap-set! s bigger)))
        (let ([heap (min-set-heap s)])
          (let sift-up ([i n])
            (if (= i 0)
              (vector-set! heap 0 id)
              (let ([parent (quotient (- i 1) 2)])
                (if (< id (vector-ref heap parent))
                  (begin
                    (vector-set! heap i (vector-ref heap parent))
                    (sift-up parent))
                  (vector-set! heap i id))))))
        (min-set-size-set! s (+ n 1)))))

  (define (min-set-pop-first! s)
    (let ([n (min-set-size s)])
      (if (= n 0)
        #f
        (let* ([heap (min-set-heap s)]
               [top (vector-ref heap 0)]
               [last (vector-ref heap (- n 1))]
               [n (- n 1)])
          (min-set-size-set! s n)
          (when (> n 0)
            (let sift-down ([i 0])
              (let* ([l (+ (* 2 i) 1)] [r (+ l 1)]
                     [smallest-child
                       (cond
                         [(>= l n) #f]
                         [(and (< r n) (< (vector-ref heap r) (vector-ref heap l))) r]
                         [else l])])
                (if (and smallest-child (< (vector-ref heap smallest-child) last))
                  (begin
                    (vector-set! heap i (vector-ref heap smallest-child))
                    (sift-down smallest-child))
                  (vector-set! heap i last)))))
          (entity-set-remove! (min-set-members s) top)
          top))))

  (define-record-type (graph-worklist %make-graph-worklist graph-worklist?)
    (fields pending-subterms pending-fn-binders dead-bindings))

  (define (make-graph-worklist)
    (%make-graph-worklist (make-worklist-queue) (make-min-set) (make-worklist-queue)))

  (define (graph-worklist-pending-subterm-len w)
    (worklist-queue-length (graph-worklist-pending-subterms w)))
  (define (graph-worklist-dead-binding-len w)
    (worklist-queue-length (graph-worklist-dead-bindings w)))
  (define (graph-worklist-pending-fn-binder-len w)
    (min-set-size (graph-worklist-pending-fn-binders w)))

  (define (graph-worklist-empty? w)
    (and (worklist-queue-empty? (graph-worklist-pending-subterms w))
      (= 0 (min-set-size (graph-worklist-pending-fn-binders w)))
      (worklist-queue-empty? (graph-worklist-dead-bindings w))))

  (define (graph-worklist-add-subterm! w subterm)
    (worklist-queue-add! (graph-worklist-pending-subterms w) subterm))

  (define (graph-worklist-add-subterms! w subterms)
    (worklist-queue-add-list! (graph-worklist-pending-subterms w) subterms))

  ;; Next pending subterm link, or #f.
  (define (graph-worklist-get-subterm! w)
    (let ([q (graph-worklist-pending-subterms w)])
      (if (worklist-queue-empty? q) #f (worklist-queue-get! q))))

  (define (graph-worklist-reconsider-function! w binder)
    (min-set-insert! (graph-worklist-pending-fn-binders w) binder))

  (define (graph-worklist-get-function-binder! w)
    (min-set-pop-first! (graph-worklist-pending-fn-binders w)))

  (define (graph-worklist-add-dead-binding! w binder)
    (worklist-queue-add! (graph-worklist-dead-bindings w) binder))

  (define (graph-worklist-get-dead-binding! w)
    (let ([q (graph-worklist-dead-bindings w)])
      (if (worklist-queue-empty? q) #f (worklist-queue-get! q))))

  ;; For every free var in VARS (list of occurrence ids), queue the owner of
  ;; each occurrence of its binder.
  (define (graph-worklist-add-occurrences! w g vars)
    (for-each
      (lambda (var)
        (let ([binder (free-binder g var)])
          (for-each-occurrence g binder
            (lambda (occ)
              (worklist-queue-add! (graph-worklist-pending-subterms w)
                (free-owner g occ))))))
      vars))

  ;; ARGS: list of free vars, FORMALS: list of bound vars, zipped; for each
  ;; pair whose arg binder satisfies IS-KNOWN-EXPR, queue the owners of the
  ;; formal's occurrences.
  (define (graph-worklist-add-known-occurrences! w g args formals is-known-expr)
    (let loop ([args args] [formals formals])
      (unless (or (null? args) (null? formals))
        (let ([binder (free-binder g (car args))])
          (when (is-known-expr binder)
            (for-each-occurrence g (car formals)
              (lambda (occ)
                (worklist-queue-add! (graph-worklist-pending-subterms w)
                  (free-owner g occ))))))
        (loop (cdr args) (cdr formals)))))

  ;; Pops subterm links until one that is still live; returns
  ;; (subterm term parent) or #f when the queue is exhausted.
  (define (graph-worklist-get-next-live-subterm! w g)
    (let loop ()
      (let ([subterm (graph-worklist-get-subterm! w)])
        (and subterm
          (let ([term (read-term-link g subterm)])
            (if term
              (list subterm term
                (read-parent-link g (term-uplink (graph-term g term))))
              (loop))))))))

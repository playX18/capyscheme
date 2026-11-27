;; Rest-argument lowering.
(library (core compiler cfg rest)
  (export lower-rest-arguments)
  (import (core) (capy) (core compiler cfg ir) (core compiler cfg graph))

  (define (alias-rest a) (car a))
  (define (alias-skip a) (cdr a))

  (define (collect-rest-aliases procedure rest)
    (define aliases (make-eqv-hashtable))
    (define changed #t)
    (let loop ()
      (when changed
        (set! changed #f)
        (for-each
          (lambda (block)
            (for-each
              (lambda (i)
                (cond
                  [(insn-prim-call? i)
                   (let ([dst (insn-prim-call-dst i)] [prim (insn-prim-call-prim i)]
                         [args (insn-prim-call-args i)])
                     (when (and (eq? prim 'cdr) (pair? args) (null? (cdr args)))
                       ;; Mutable uvars may redefine a home many times
                       ;; (`v = cdr(v)').  Keep the first alias only so the
                       ;; fixed point cannot climb `skip' forever.
                       (unless (hashtable-contains? aliases dst)
                         (let ([alias (rest-alias-for-operand (car args) rest aliases)])
                           (when alias
                             (hashtable-set! aliases dst
                               (cons (alias-rest alias) (+ (alias-skip alias) 1)))
                             (set! changed #t))))))]
                  [(insn-assign? i)
                   (let ([dst (insn-assign-dst i)])
                     (unless (hashtable-contains? aliases dst)
                       (let ([alias (rest-alias-for-operand (insn-assign-src i) rest aliases)])
                         (when alias
                           (hashtable-set! aliases dst alias)
                           (set! changed #t)))))]
                  [else #f]))
              (block-instructions block)))
          (cfg-proc-blocks procedure))
        (loop)))
    aliases)

  (define (rest-alias-for-operand operand rest aliases)
    (and (operand-local? operand)
      (let ([v (operand-local-uvar operand)])
        (if (eqv? v rest)
          (cons rest 0)
          (hashtable-ref aliases v #f)))))

  (define (rest-rewrite-for-prim prim args rest aliases)
    (and (pair? args) (null? (cdr args))
      (let ([alias (rest-alias-for-operand (car args) rest aliases)])
        (and alias
          (case prim
            [(cdr) (list 'cdr)]
            [(car) (list 'ref alias)]
            [(length) (list 'length alias)]
            [(null?) (list 'predicate alias 'Null)]
            [(pair?) (list 'predicate alias 'Pair)]
            [(list?) (list 'predicate alias 'List)]
            [else #f])))))

  (define (mentions-rest? operands rest aliases)
    (exists (lambda (o) (rest-alias-for-operand o rest aliases)) operands))

  (define (has-incompatible-rest-use? procedure rest aliases loop-set)
    ;; The extra arguments sit in the frame above the registers, where the
    ;; frame of a non-tail call goes.  They can only be read lazily up to the
    ;; first such call, anything later needs the list.
    (define after-call (blocks-after-calls procedure))
    (let block-loop ([blocks (cfg-proc-blocks procedure)])
      (if (null? blocks)
        #f
        (let* ([block (car blocks)]
               [is-loop-block (or (hashtable-contains? loop-set (block-id block))
                                  (hashtable-contains? after-call (block-id block)))])
          (or
            (exists
              (lambda (i)
                (and (mentions-rest? (insn-uses i) rest aliases)
                  (or is-loop-block
                    (not
                      (cond
                        [(insn-prim-call? i)
                         (rest-rewrite-for-prim (insn-prim-call-prim i) (insn-prim-call-args i)
                           rest aliases)]
                        ;; Moves of rest/aliases are fine, they do not force RestToList.
                        [(insn-assign? i) #t]
                        [else #f])))))
              (block-instructions block))
            (let ([term (block-terminator block)])
              (and (mentions-rest? (term-uses term) rest aliases)
                (not
                  (and (not is-loop-block)
                    (term-branch-prim? term)
                    (rest-rewrite-for-prim (term-branch-prim-prim term)
                      (term-branch-prim-args term) rest aliases)
                    #t))))
            (block-loop (cdr blocks)))))))

  ;; Blocks reachable from the return point of some non-tail call (a set).
  (define (blocks-after-calls procedure)
    (define successors (make-eqv-hashtable))
    (define reached (make-eqv-hashtable))
    (for-each
      (lambda (b) (hashtable-set! successors (block-id b) (term-successors (block-terminator b))))
      (cfg-proc-blocks procedure))
    (let loop ([stack
                (let collect ([bs (cfg-proc-blocks procedure)] [acc '()])
                  (cond
                    [(null? bs) (reverse acc)]
                    [(term-call? (block-terminator (car bs)))
                     (collect (cdr bs)
                       (cons (return-point-block (term-call-ret (block-terminator (car bs))))
                         acc))]
                    [else (collect (cdr bs) acc)]))])
      (unless (null? stack)
        (let ([b (car stack)] [rest (cdr stack)])
          (if (hashtable-contains? reached b)
            (loop rest)
            (begin
              (hashtable-set! reached b #t)
              (loop (append (reverse (hashtable-ref successors b '())) rest)))))))
    reached)

  (define (find-block procedure id)
    (let loop ([bs (cfg-proc-blocks procedure)])
      (cond
        [(null? bs) (error 'lower-rest-arguments "procedure should contain entry block")]
        [(eqv? (block-id (car bs)) id) (car bs)]
        [else (loop (cdr bs))])))

  (define (insert-rest-to-list! procedure rest)
    (let ([entry (find-block procedure (cfg-proc-entry procedure))])
      (block-instructions-set! entry
        (cons (make-insn-rest-to-list rest rest (block-source entry))
          (block-instructions entry)))))

  (define (max-uvar procedure)
    (define m #f)
    (define (bump! v) (set! m (if m (max m v) v)))
    (for-each bump! (cfg-proc-params procedure))
    (when (cfg-proc-variadic procedure) (bump! (cfg-proc-variadic procedure)))
    (bump! (cfg-proc-binding procedure))
    (for-each bump! (cfg-proc-free-vars procedure))
    (for-each
      (lambda (block)
        (for-each
          (lambda (i)
            (let ([d (insn-def i)]) (when d (bump! d)))
            (for-each (lambda (o) (when (operand-local? o) (bump! (operand-local-uvar o))))
              (insn-uses i)))
          (block-instructions block))
        (for-each (lambda (o) (when (operand-local? o) (bump! (operand-local-uvar o))))
          (term-uses (block-terminator block))))
      (cfg-proc-blocks procedure))
    m)

  (define (rewrite-rest-uses! procedure rest aliases)
    (define next (let ([m (max-uvar procedure)]) (if m (+ m 1) 0)))
    (for-each
      (lambda (block)
        (block-instructions-set! block
          (let loop ([is (block-instructions block)] [acc '()])
            (if (null? is)
              (reverse acc)
              (let ([i (car is)])
                (if (insn-prim-call? i)
                  (let* ([dst (insn-prim-call-dst i)]
                         [source (insn-prim-call-source i)]
                         [rw (rest-rewrite-for-prim (insn-prim-call-prim i)
                               (insn-prim-call-args i) rest aliases)])
                    (cond
                      [(not rw) (loop (cdr is) (cons i acc))]
                      [(eq? (car rw) 'cdr) (loop (cdr is) acc)]
                      [(eq? (car rw) 'ref)
                       (loop (cdr is)
                         (cons (make-insn-rest-ref dst (alias-rest (cadr rw))
                                 (alias-skip (cadr rw)) source)
                           acc))]
                      [(eq? (car rw) 'length)
                       (loop (cdr is)
                         (cons (make-insn-rest-length dst (alias-rest (cadr rw))
                                 (alias-skip (cadr rw)) source)
                           acc))]
                      [else
                       (loop (cdr is)
                         (cons (make-insn-rest-predicate dst (alias-rest (cadr rw))
                                 (caddr rw) (alias-skip (cadr rw)) source)
                           acc))]))
                  (loop (cdr is) (cons i acc)))))))
        (let ([term (block-terminator block)])
          (when (term-branch-prim? term)
            (let ([rw (rest-rewrite-for-prim (term-branch-prim-prim term)
                        (term-branch-prim-args term) rest aliases)])
              (when (and rw (eq? (car rw) 'predicate))
                (let ([dst next] [alias (cadr rw)])
                  (set! next (+ next 1))
                  (block-instructions-set! block
                    (append (block-instructions block)
                      (list (make-insn-rest-predicate dst (alias-rest alias) (caddr rw)
                              (alias-skip alias) (block-source block)))))
                  (block-terminator-set! block
                    (make-term-branch (make-operand-local dst)
                      (term-branch-prim-consequent term)
                      (term-branch-prim-alternative term)
                      (term-branch-prim-hints term)))))))))
      (cfg-proc-blocks procedure)))

  (define (lower-rest-arguments procedure)
    (let ([rest (cfg-proc-variadic procedure)])
      (when rest
        (let* ([aliases (collect-rest-aliases procedure rest)]
               [loop-set (loop-blocks procedure)])
          (if (has-incompatible-rest-use? procedure rest aliases loop-set)
            (insert-rest-to-list! procedure rest)
            (rewrite-rest-uses! procedure rest aliases))))
      procedure))
)

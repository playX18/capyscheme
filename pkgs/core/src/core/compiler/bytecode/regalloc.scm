;; Liveness analysis and linear-scan register allocation.
;;
;; A frame is laid out as
;;   r0 return address, r1 closure (the procedure's binding), r2.. parameters,
;;   then allocated registers, then temporaries, then the rest area.
;;
;; (allocate-registers procedure) -> regalloc record with
;;   ra-blocks         eqv hashtable BlockId -> block          (lookup only)
;;   ra-order          list of BlockId: reverse post-order from the entry with
;;                     blocks ending in `raise' moved (stably) to the end
;;   ra-order-index    eqv hashtable BlockId -> index in order  (lookup only)
;;   ra-live-in        eqv hashtable BlockId -> uvar set (sorted list)
;;   ra-loop-headers   eqv hashtable BlockId -> #t              (lookup only)
;;   ra-used           eqv hashtable UVar -> #t                 (lookup only)
;;   ra-registers      eqv hashtable UVar -> register           (lookup only)
;;   ra-tmp-base       first register above the allocated ones
;;   ra-cnt-register   register of the rest-argument count, or #f

(library (core compiler bytecode regalloc)
  (export
    allocate-registers regalloc?
    ra-blocks ra-order ra-order-index ra-live-in ra-loop-headers ra-used ra-registers
    ra-tmp-base ra-cnt-register
    insn-use-vars term-use-vars lower-successors live-after-index
    uset-from-list uset-union uset-remove uset-member?)
  (import (core) (capy) (core compiler cfg ir))

  (define-record-type regalloc
    (fields blocks order order-index live-in loop-headers used registers tmp-base
      cnt-register))

  (define (ra-blocks r) (regalloc-blocks r))
  (define (ra-order r) (regalloc-order r))
  (define (ra-order-index r) (regalloc-order-index r))
  (define (ra-live-in r) (regalloc-live-in r))
  (define (ra-loop-headers r) (regalloc-loop-headers r))
  (define (ra-used r) (regalloc-used r))
  (define (ra-registers r) (regalloc-registers r))
  (define (ra-tmp-base r) (regalloc-tmp-base r))
  (define (ra-cnt-register r) (regalloc-cnt-register r))

  (define (uset-from-list l)
    (let loop ([s (list-sort < l)] [acc '()])
      (cond
        [(null? s) (reverse acc)]
        [(and (pair? acc) (= (car acc) (car s))) (loop (cdr s) acc)]
        [else (loop (cdr s) (cons (car s) acc))])))

  (define (uset-union a b)
    (cond
      [(null? a) b]
      [(null? b) a]
      [(< (car a) (car b)) (cons (car a) (uset-union (cdr a) b))]
      [(> (car a) (car b)) (cons (car b) (uset-union a (cdr b)))]
      [else (cons (car a) (uset-union (cdr a) (cdr b)))]))

  (define (uset-remove a x)
    (cond
      [(null? a) '()]
      [(< (car a) x) (cons (car a) (uset-remove (cdr a) x))]
      [(= (car a) x) (cdr a)]
      [else a]))

  ;; A less B
  (define (uset-minus a b)
    (let loop ([a a] [b b] [acc '()])
      (cond
        [(null? a) (reverse acc)]
        [(null? b) (append (reverse acc) a)]
        [(< (car a) (car b)) (loop (cdr a) b (cons (car a) acc))]
        [(> (car a) (car b)) (loop a (cdr b) acc)]
        [else (loop (cdr a) (cdr b) acc)])))

  (define (uset-member? a x)
    (cond
      [(null? a) #f]
      [(< (car a) x) (uset-member? (cdr a) x)]
      [else (= (car a) x)]))

  (define (locals-of operands)
    (let loop ([os operands] [acc '()])
      (cond
        [(null? os) (reverse acc)]
        [(operand-local? (car os)) (loop (cdr os) (cons (operand-local-uvar (car os)) acc))]
        [else (loop (cdr os) acc)])))

  ;; `instruction_uses': the rest instructions use no register.
  (define (insn-use-vars i)
    (if (or (insn-rest-to-list? i) (insn-rest-ref? i) (insn-rest-length? i)
            (insn-rest-predicate? i))
      '()
      (locals-of (insn-uses i))))

  (define (term-use-vars t) (locals-of (term-uses t)))

  ;; `Lower::successors': for branches the alternative comes first.
  (define (lower-successors t)
    (define (local-of target) (target-local-successor target))
    (define (compact l) (filter (lambda (x) x) l))
    (cond
      [(term-branch? t)
       (compact (list (local-of (term-branch-alternative t))
                  (local-of (term-branch-consequent t))))]
      [(term-branch-prim? t)
       (compact (list (local-of (term-branch-prim-alternative t))
                  (local-of (term-branch-prim-consequent t))))]
      [else (term-successors t)]))

  (define (block-of blocks id)
    (or (hashtable-ref blocks id #f)
      (assertion-violation 'lower "block is not in the procedure" id)))

 
  (define (order-blocks procedure blocks)
    (define entry (cfg-proc-entry procedure))
    (define visited (make-eqv-hashtable))
    (define (succs id) (lower-successors (block-terminator (block-of blocks id))))
    (hashtable-set! visited entry #t)
    ;; Iterative DFS
    (let ([rpo
           (let loop ([stack (list (cons entry (succs entry)))] [post '()])
             (if (null? stack)
               post
               (let* ([frame (car stack)] [rest (cdr frame)])
                 (if (pair? rest)
                   (let ([s (car rest)])
                     (set-cdr! frame (cdr rest))
                     (if (and (hashtable-contains? blocks s) (not (hashtable-ref visited s #f)))
                       (begin
                         (hashtable-set! visited s #t)
                         (loop (cons (cons s (succs s)) stack) post))
                       (loop stack post)))
                   (loop (cdr stack) (cons (car frame) post))))))])
      (let loop ([ids rpo] [hot '()] [cold '()])
        (cond
          [(null? ids) (append (reverse hot) (reverse cold))]
          [(and (not (eqv? (car ids) entry))
                (term-raise? (block-terminator (block-of blocks (car ids)))))
           (loop (cdr ids) hot (cons (car ids) cold))]
          [else (loop (cdr ids) (cons (car ids) hot) cold)]))))

 
  (define (compute-liveness order blocks)
    (define live-in (make-eqv-hashtable))
    (define predecessors (make-eqv-hashtable))
    (for-each (lambda (id) (hashtable-set! live-in id '())) order)
    (for-each
      (lambda (id)
        (for-each
          (lambda (s)
            (hashtable-set! predecessors s
              (append (hashtable-ref predecessors s '()) (list id))))
          (term-successors (block-terminator (block-of blocks id)))))
      order)
    ;; Per block, (gen . kill): the uses before any definition in the block, and the
    ;; definitions, so that live-in = gen + (live-out - kill). That is the fold of the
    ;; instructions over live-out (induction over the instructions from the end), done
    ;; once instead of on every visit of the block.
    (define summaries (make-eqv-hashtable))
    (for-each
      (lambda (id)
        (let ([block (block-of blocks id)])
          (let loop ([insns (reverse (block-instructions block))]
                     [gen (uset-from-list (term-use-vars (block-terminator block)))]
                     [kill '()])
            (if (null? insns)
              (hashtable-set! summaries id (cons gen kill))
              (let ([defs (uset-from-list (insn-defs (car insns)))])
                (loop (cdr insns)
                  (uset-union (uset-minus gen defs) (uset-from-list (insn-use-vars (car insns))))
                  (uset-union kill defs)))))))
      order)
    ;; FIFO worklist as front/back lists.
    (let ([front (reverse order)] [back '()] [queued (make-eqv-hashtable)])
      (for-each (lambda (id) (hashtable-set! queued id #t)) order)
      (let loop ()
        (when (and (null? front) (pair? back))
          (set! front (reverse back))
          (set! back '()))
        (when (pair? front)
          (let ([id (car front)])
            (set! front (cdr front))
            (hashtable-delete! queued id)
            (let* ([block (block-of blocks id)]
                   [term (block-terminator block)]
                   [live-out (fold-left
                               (lambda (acc s)
                                 (let ([sl (hashtable-ref live-in s #f)])
                                   (if sl (uset-union acc sl) acc)))
                               '()
                               (term-successors term))]
                   [summary (hashtable-ref summaries id #f)]
                   [live (uset-union (car summary) (uset-minus live-out (cdr summary)))])
              (unless (equal? live (hashtable-ref live-in id '()))
                (hashtable-set! live-in id live)
                (for-each
                  (lambda (p)
                    (unless (hashtable-ref queued p #f)
                      (hashtable-set! queued p #t)
                      (set! back (cons p back))))
                  (hashtable-ref predecessors id '())))))
          (loop))))
    live-in)

  ;; Uvars live just after instruction `index' of `block'.
  (define (live-after-index blocks live-in id index)
    (let* ([block (block-of blocks id)]
           [term (block-terminator block)]
           [live0 (fold-left
                    (lambda (acc s)
                      (let ([sl (hashtable-ref live-in s #f)])
                        (if sl (uset-union acc sl) acc)))
                    '()
                    (term-successors term))]
           [live1 (uset-union live0 (uset-from-list (term-use-vars term)))]
           [after (list-tail (block-instructions block) (+ index 1))])
      (fold-right
        (lambda (insn live)
          (let ([live (fold-left uset-remove live (insn-defs insn))])
            (uset-union live (uset-from-list (insn-use-vars insn)))))
        live1
        after)))

  (define (allocate-registers procedure)
    (define blocks (make-eqv-hashtable))
    (for-each (lambda (b) (hashtable-set! blocks (block-id b) b)) (cfg-proc-blocks procedure))
    (let* ([order (order-blocks procedure blocks)]
           [order-index (make-eqv-hashtable)]
           [loop-headers (make-eqv-hashtable)])
      (let loop ([ids order] [i 0])
        (unless (null? ids)
          (hashtable-set! order-index (car ids) i)
          (loop (cdr ids) (+ i 1))))
      (let loop ([ids order] [i 0])
        (unless (null? ids)
          (for-each
            (lambda (s)
              (let ([si (hashtable-ref order-index s #f)])
                (when (and si (<= si i))
                  (hashtable-set! loop-headers s #t))))
            (lower-successors (block-terminator (block-of blocks (car ids)))))
          (loop (cdr ids) (+ i 1))))
      (let* ([live-in (compute-liveness order blocks)]
             [used (make-eqv-hashtable)])
        (for-each
          (lambda (id)
            (let ([block (block-of blocks id)])
              (for-each
                (lambda (i) (for-each (lambda (v) (hashtable-set! used v #t)) (insn-use-vars i)))
                (block-instructions block))
              (for-each (lambda (v) (hashtable-set! used v #t))
                (term-use-vars (block-terminator block)))))
          order)
        (let-values ([(registers tmp-base cnt-register)
                      (linear-scan procedure blocks order order-index live-in)])
          (make-regalloc blocks order order-index live-in loop-headers used registers
            tmp-base cnt-register)))))

  ;;; A binary min-heap ordered by LESS?
  (define-record-type heap (fields less? (mutable items) (mutable size)))
  (define (new-heap less?) (make-heap less? (make-vector 16 #f) 0))
  (define (heap-empty? h) (= (heap-size h) 0))
  (define (heap-min h) (vector-ref (heap-items h) 0))
  (define (heap-push! h x)
    (let ([n (heap-size h)])
      (when (= n (vector-length (heap-items h)))
        (let ([bigger (make-vector (* 2 n) #f)])
          (let copy ([i 0])
            (when (< i n)
              (vector-set! bigger i (vector-ref (heap-items h) i))
              (copy (+ i 1))))
          (heap-items-set! h bigger)))
      (let ([v (heap-items h)] [less? (heap-less? h)])
        (let up ([i n])
          (if (= i 0)
            (vector-set! v 0 x)
            (let ([parent (div (- i 1) 2)])
              (if (less? x (vector-ref v parent))
                (begin (vector-set! v i (vector-ref v parent)) (up parent))
                (vector-set! v i x))))))
      (heap-size-set! h (+ n 1))))
  (define (heap-pop! h)
    (let* ([v (heap-items h)]
           [less? (heap-less? h)]
           [top (vector-ref v 0)]
           [n (- (heap-size h) 1)]
           [last (vector-ref v n)])
      (vector-set! v n #f)
      (heap-size-set! h n)
      (when (> n 0)
        (let down ([i 0])
          (let* ([l (+ (* 2 i) 1)] [r (+ l 1)]
                 [c (cond
                      [(>= l n) #f]
                      [(and (< r n) (less? (vector-ref v r) (vector-ref v l))) r]
                      [else l])])
            (if (and c (less? (vector-ref v c) last))
              (begin (vector-set! v i (vector-ref v c)) (down c))
              (vector-set! v i last)))))
      top))

  (define (linear-scan procedure blocks order order-index live-in)
    (define binding (cfg-proc-binding procedure))
    (define params (cfg-proc-params procedure))
    ;; One interval per uvar: first definition to last use, widened to whole
    ;; blocks it is live through.  `interval-vars' keeps first-insertion order,
    ;; the vector holds mutable (start . end) pairs by uvar: the uvars of a
    ;; procedure are numbered from 0 (see cfg/lower.scm).
    (define intervals (make-vector 64 #f))
    (define (interval-of var)
      (and (< var (vector-length intervals)) (vector-ref intervals var)))
    (define (set-interval! var iv)
      (when (>= var (vector-length intervals))
        (let ([bigger (make-vector (max (+ var 1) (* 2 (vector-length intervals))) #f)])
          (let copy ([i 0])
            (when (< i (vector-length intervals))
              (vector-set! bigger i (vector-ref intervals i))
              (copy (+ i 1))))
          (set! intervals bigger)))
      (vector-set! intervals var iv))
    (define interval-vars '())
    (define (extend! var position)
      (unless (eqv? var binding)
        (let ([iv (interval-of var)])
          (if iv
            (begin
              (when (< position (car iv)) (set-car! iv position))
              (when (> position (cdr iv)) (set-cdr! iv position)))
            (begin
              (set-interval! var (cons position position))
              (set! interval-vars (cons var interval-vars)))))))
    ;; Assign copies whose source dies at the copy. Later copies of the same
    ;; destination replace earlier ones
    (define hints (make-eqv-hashtable))
    (for-each (lambda (p) (extend! p 0)) params)
    (let loop ([ids order] [position 1] [index 0])
      (unless (null? ids)
        (let* ([block (block-of blocks (car ids))]
               [start position])
          (for-each (lambda (v) (extend! v start)) (hashtable-ref live-in (car ids) '()))
          (let iloop ([is (block-instructions block)] [position position])
            (cond
              [(pair? is)
               (let ([insn (car is)])
                 (for-each (lambda (v) (extend! v position)) (insn-use-vars insn))
                 (for-each
                   (lambda (def)
                     (if (or (insn-receive? insn) (insn-receive-rest? insn))
                       (extend! def start)
                       (extend! def position)))
                   (insn-defs insn))
                 (when (and (insn-assign? insn) (operand-local? (insn-assign-src insn)))
                   (hashtable-set! hints (insn-assign-dst insn)
                     (cons (operand-local-uvar (insn-assign-src insn)) position)))
                 (iloop (cdr is) (+ position 1)))]
              [else
               (let ([term (block-terminator block)])
                 (for-each (lambda (v) (extend! v position)) (term-use-vars term))
                 ;; What is live into a successor is live out of this block: defined
                 ;; in it, or live into it, so the interval already starts before
                 ;; here. A successor that comes later extends it past here at its
                 ;; own start, so only a back edge can make this the end.
                 (for-each
                   (lambda (s)
                     (let ([live (hashtable-ref live-in s #f)]
                           [si (hashtable-ref order-index s #f)])
                       (when (and live si (<= si index))
                         (for-each (lambda (v) (extend! v position)) live))))
                   (term-successors term))
                 (loop (cdr ids) (+ position 1) (+ index 1)))])))))
    (let* ([nparams (length params)]
           [first-free0 (+ 2 nparams)]
           [variadic? (and (cfg-proc-variadic procedure) #t)]
           [cnt-register (and variadic? first-free0)]
           [first-free (if variadic? (+ first-free0 1) first-free0)]
           [registers (make-eqv-hashtable)]
           [in-use (make-eqv-hashtable)]
           ;; The active intervals: register -> end, and a heap of (end . register) to
           ;; expire them. A register has one active interval at a time, so a heap entry
           ;; whose end is no longer the one of its register is stale.
           [active (make-eqv-hashtable)]
           [expiry (new-heap (lambda (a b) (< (car a) (car b))))]
           ;; The registers from FIRST-FREE up that were freed, and the first one never
           ;; used: those in use above FIRST-FREE are always FIRST-FREE..NEXT-UNUSED-1
           ;; less the freed ones, so the lowest free register is the lower of the two.
           [freed (new-heap <)]
           [next-unused first-free])
      (define (activate! register end)
        (hashtable-set! active register end)
        (heap-push! expiry (cons end register)))
      (define (free! register)
        (hashtable-delete! in-use register)
        (when (>= register first-free) (heap-push! freed register)))
      (define (expire-before! start)
        (let loop ()
          (when (and (not (heap-empty? expiry)) (< (car (heap-min expiry)) start))
            (let* ([entry (heap-pop! expiry)]
                   [register (cdr entry)])
              (when (eqv? (hashtable-ref active register #f) (car entry))
                (hashtable-delete! active register)
                (free! register)))
            (loop))))
      (define (lowest-free!)
        (let loop ()
          (cond
            [(and (not (heap-empty? freed)) (hashtable-ref in-use (heap-min freed) #f))
              (heap-pop! freed)
              (loop)]
            [(heap-empty? freed)
              (let ([register next-unused])
                (set! next-unused (+ next-unused 1))
                register)]
            [else (heap-pop! freed)])))
      (hashtable-set! registers binding 1)
      (let loop ([ps params] [index 0])
        (unless (null? ps)
          (let ([register (+ 2 index)])
            (hashtable-set! registers (car ps) register)
            (let ([iv (interval-of (car ps))])
              (activate! register (if iv (cdr iv) 0)))
            (hashtable-set! in-use register #t))
          (loop (cdr ps) (+ index 1))))
      (let* ([pending
              (list-sort
                (lambda (a b)
                  (or (< (cadr a) (cadr b))
                      (and (= (cadr a) (cadr b)) (< (car a) (car b)))))
                (let collect ([vs interval-vars] [acc '()])
                  (cond
                    [(null? vs) acc]
                    [(hashtable-ref registers (car vs) #f) (collect (cdr vs) acc)]
                    [else
                     (let ([iv (interval-of (car vs))])
                       (collect (cdr vs) (cons (list (car vs) (car iv) (cdr iv)) acc)))])))]
             [high first-free])
        (for-each
          (lambda (entry)
            (let ([var (car entry)] [start (cadr entry)] [end (caddr entry)])
              (expire-before! start)
              ;; A copy whose source ends exactly here may take over its
              ;; register, the copy then costs nothing.
              (let* ([chosen
                      (let ([hint (hashtable-ref hints var #f)])
                        (and hint
                             (= (cdr hint) start)
                             (let ([register (hashtable-ref registers (car hint) #f)])
                               (and register
                                    (>= register first-free)
                                    (let ([siv (interval-of (car hint))])
                                      (and siv (= (cdr siv) start)))
                                    (hashtable-contains? active register)
                                    (begin
                                      (hashtable-delete! active register)
                                      register)))))]
                     [register (or chosen (lowest-free!))])
                (hashtable-set! in-use register #t)
                (activate! register end)
                (hashtable-set! registers var register)
                (set! high (max high (+ register 1))))))
          pending)
        (values registers high cnt-register))))
)

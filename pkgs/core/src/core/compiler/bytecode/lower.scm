;; Lower a CFG program to (symbolic) bytecode.
;;
;; Frame layout:
;;   r0 return address, r1 the closure (the procedure's `binding`), r2..
;;   parameters then allocated registers, temporaries (constants materialized
;;   for one instruction), and the rest area of a variadic procedure.  A
;;   non-tail call puts the callee frame at `base`, two above the highest
;;   register live after the call; r[base-1] stays free.  Registers live
;;   across the call are the mask of the return point.
;;
;; Exports:
;;   (compile-program program)        program: cfg `program'
;;        -> compiled-program (see (core compiler bytecode types)): the list of
;;           compiled procedures (code-id, symbolic instructions, constants,
;;           pool words, arity, frame-size, name, meta, source, srcmap, spans,
;;           metadata-counts, code bytevector) and the entry index.
;;   (compile-procedure procedure)    one procedure (Rust `compile_procedure')
;;   dense-span-for-keys
(library (core compiler bytecode lower)
  (export compile-program compile-procedure dense-span-for-keys)
  (import (core) (capy)
    (core compiler cfg ir)
    (core compiler bytecode prims)
    (core compiler bytecode regalloc)
    (core compiler bytecode types)
    (core compiler bytecode encode)
    (only (core thread) go thread-join))

  (define mv-error 0)
  (define mv-ignore 1)
  (define case-no-rest #xFFFFFFFF)
  (define switch-dense-flag #x80000000)
  (define switch-dense-max-span 256)
  (define switch-dense-max-sparse-multiple 4)
  (define u32-mask #xFFFFFFFF)

  (define-record-type gvec (fields (mutable vec) (mutable n)))
  (define (new-gvec) (make-gvec (make-vector 16 #f) 0))
  (define (gvec-length g) (gvec-n g))
  (define (gvec-ref g i) (vector-ref (gvec-vec g) i))
  (define (gvec-set! g i x) (vector-set! (gvec-vec g) i x))
  (define (gvec-push! g x)
    (let ([n (gvec-n g)] [v (gvec-vec g)])
      (when (= n (vector-length v))
        (let ([nv (make-vector (* 2 n) #f)])
          (let loop ([i 0])
            (when (< i n) (vector-set! nv i (vector-ref v i)) (loop (+ i 1))))
          (gvec-vec-set! g nv)))
      (vector-set! (gvec-vec g) n x)
      (gvec-n-set! g (+ n 1))))
  (define (gvec->list g)
    (let loop ([i (- (gvec-n g) 1)] [acc '()])
      (if (< i 0) acc (loop (- i 1) (cons (gvec-ref g i) acc)))))

  ;; --- arena: constant pool and metadata shared by one code block -----------
  (define-record-type arena
    (fields constants value-index code-index cell-index pool mask-index
      (mutable next-label)))
  (define (new-arena)
    (make-arena (new-gvec) (make-eq-hashtable) (make-hashtable equal-hash equal?) (make-eq-hashtable)
      (new-gvec) (make-hashtable equal-hash equal?) 0))
  (define (arena-new-label! a)
    (let ([l (arena-next-label a)])
      (arena-next-label-set! a (+ l 1))
      l))

  ;; Result of lowering one procedure before linking.
  (define-record-type emitted
    (fields atoms sources switch-fixups size arity name meta source))

  (define (immediate-bits v)
    ;; Raw bits of a non-cell, non-int32 immediate that fits u32, else #f.
    ;; (lower.rs `load_const': `!value.is_cell() && value.bits() <= u32::MAX'.)
    ;; GAP: `void', `bwp' and the internal undefined sentinel cannot be
    ;; recognized from Scheme; they are pooled like cells instead.
    (cond
      [(eq? v #f) 6]
      [(eq? v #t) 7]
      [(null? v) 2]
      [(char? v)
       (let ([c (char->integer v)])
         (and (<= c #xFFFF) (+ (* c 65536) #x82)))]
      [(unspecified? v) 10]   ; `(if #f #f)' is Value::undefined(), bits 10
      [(eof-object? v) #x3E]
      [else #f]))

  (define (dense-span-for-keys keys)
    ;; (min . span) when sorted, deduplicated int-switch `keys' use the dense
    ;; layout.
    (and (pair? keys)
      (let* ([mn (car keys)]
             [mx (let loop ([k keys]) (if (null? (cdr k)) (car k) (loop (cdr k))))]
             [span (+ (- mx mn) 1)])
        (and (<= span switch-dense-max-span)
          (<= span (* switch-dense-max-sparse-multiple (length keys)))
          (cons mn span)))))

  (define (sym-op kind)
    (case kind
      [(null) 'jump-if-null] [(nnull) 'jump-if-not-null]
      [(pair) 'jump-if-pair] [(npair) 'jump-if-not-pair]
      [(fixnum) 'jump-if-fixnum] [(nfixnum) 'jump-if-not-fixnum]
      [(undefined) 'jump-if-undefined] [(nundefined) 'jump-if-not-undefined]
      [else (assertion-violation 'lower "two-register branch kind in a type test" kind)]))

  (define (cmp-op kind)
    (case kind
      [(eq) 'jump-if-eq] [(neq) 'jump-if-not-eq]
      [(lt) 'jump-if-less] [(le) 'jump-if-less-equal]
      [(numeq) 'jump-if-number-equal] [(ge) 'jump-if-greater-equal]
      [(gt) 'jump-if-greater] [(nlt) 'jump-if-not-less]
      [(nle) 'jump-if-not-less-equal] [(nnumeq) 'jump-if-not-number-equal]
      [(nge) 'jump-if-not-greater-equal] [(ngt) 'jump-if-not-greater]
      [else (assertion-violation 'lower "one-register branch kind in a comparison" kind)]))

  (define (unspecific) (if #f #f))

  ;; =========================================================================
  ;; Lowering one procedure
  ;; =========================================================================
  ;; Rust: `Lower::new' + `run' (`order_blocks', `compute_liveness',
  ;; `allocate_registers', `emit_procedure') + `emit_parts'.
  (define (lower-procedure procedure arena)
    (define ra (allocate-registers procedure))
    (define blocks (ra-blocks ra))
    (define order (ra-order ra))
    (define live-in (ra-live-in ra))
    (define loop-headers (ra-loop-headers ra))
    (define used (ra-used ra))
    (define registers (ra-registers ra))
    (define tmp-base (ra-tmp-base ra))
    (define cnt-register (ra-cnt-register ra))
    (define binding (cfg-proc-binding procedure))
    (define nparams (length (cfg-proc-params procedure)))
    (define variadic? (and (cfg-proc-variadic procedure) #t))

    (define max-tmp 0)
    (define extent 0)
    (define atoms (new-gvec))
    (define labels (make-eqv-hashtable))            ; BlockId -> label (lookup only)
    (define sources '())                             ; newest first: (atom-index . source)
    (define rest-fixups '())                         ; atom indices
    (define switch-fixups '())                       ; newest first
    (define slot-stubs '())                          ; newest first
    (define insn-vectors (make-eqv-hashtable))       ; BlockId -> vector of insns

    (define (block-of id) (hashtable-ref blocks id #f))
    (define (insn-vector id)
      (or (hashtable-ref insn-vectors id #f)
        (let ([v (list->vector (block-instructions (block-of id)))])
          (hashtable-set! insn-vectors id v)
          v)))
    (define (used? var) (hashtable-ref used var #f))
    (define (new-label) (arena-new-label! arena))

    (define (push-atom! a) (gvec-push! atoms a))
    (define (branch! i) (push-atom! i))
    (define (push-label! l) (push-atom! (list 'label l)))

    (define (try-fuse next)
      (let ([n (gvec-length atoms)])
        (and (> n 0)
          (let* ([prev (gvec-ref atoms (- n 1))]
                 [pop (car prev)]
                 [nop (car next)]
                 [fused
                  (cond
                    [(and (eq? pop 'move) (eq? nop 'move))
                     ;; move-two dst1 src1 dst2 src2
                     (list 'move-two (cadr prev) (caddr prev) (cadr next) (caddr next))]
                    [(and (eq? pop 'add-immediate) (eq? nop 'move)
                          (eqv? (caddr next) (cadr prev)))
                     ;; add-immediate dst src value ; move dst' dst
                     ;; -> add-immediate-move tmp src value dst'
                     (list 'add-immediate-move (cadr prev) (caddr prev) (cadddr prev)
                       (cadr next))]
                    [(and (eq? pop 'vector-ref) (eq? nop 'mul))
                     ;; vector-ref dst vector index ; mul dst lhs rhs
                     (list 'vector-ref-mul (cadr prev) (caddr prev) (cadddr prev)
                       (cadr next) (caddr next) (cadddr next))]
                    [else #f])])
            (and fused
              (let ([fused-at (- n 1)])
                (let loop ([ss sources])
                  (when (and (pair? ss) (> (car (car ss)) fused-at))
                    (set-car! (car ss) fused-at)
                    (loop (cdr ss))))
                (gvec-set! atoms fused-at fused)
                #t))))))

    (define (ins i)
      (unless (try-fuse i) (push-atom! i)))

    (define (block-label id)
      (or (hashtable-ref labels id #f)
        (let ([l (new-label)])
          (hashtable-set! labels id l)
          l)))

    (define (note-source! source)
      (set! sources (cons (cons (gvec-length atoms) source) sources)))

    (define (bump-extent! e) (set! extent (max extent e)))

    (define (reg var)
      (or (hashtable-ref registers var #f)
        (assertion-violation 'lower "uvar has no register" var)))

    (define (temp index)
      (set! max-tmp (max max-tmp (+ index 1)))
      (+ tmp-base index))

    (define (mov dst src)
      (unless (eqv? dst src)
        (ins (list 'move dst src))))

    (define (value-constant value)
      (or (hashtable-ref (arena-value-index arena) value #f)
        (let ([index (gvec-length (arena-constants arena))])
          (gvec-push! (arena-constants arena) (cons 'value value))
          (hashtable-set! (arena-value-index arena) value index)
          index)))

    (define (code-constant code)
      (or (hashtable-ref (arena-code-index arena) code #f)
        (let ([index (gvec-length (arena-constants arena))])
          (gvec-push! (arena-constants arena) (cons 'code code))
          (hashtable-set! (arena-code-index arena) code index)
          index)))

    (define (cell-constant key)
      (or (hashtable-ref (arena-cell-index arena) key #f)
        (let ([index (gvec-length (arena-constants arena))])
          (gvec-push! (arena-constants arena) (cons 'cell key))
          (hashtable-set! (arena-cell-index arena) key index)
          index)))

    (define (load-const dst value)
      (cond
        [(fixnum? value) (ins (list 'load-immediate dst value))]
        [(immediate-bits value) => (lambda (bits) (ins (list 'load-raw dst bits)))]
        [else
         (let ([index (value-constant value)])
           (ins (list 'load-constant dst index)))]))

    ;; Register holding `o', materializing a constant in temporary `slot'.
    (define (operand o slot)
      (if (operand-local? o)
        (reg (operand-local-uvar o))
        (let ([t (temp slot)])
          (load-const t (operand-constant-value o))
          t)))

    ;; Src: (reg . n) or (const . value)
    (define (src-of o)
      (if (operand-local? o)
        (cons 'reg (reg (operand-local-uvar o)))
        (cons 'const (operand-constant-value o))))

    ;; Perform `moves' ((src . dst) ...) as one parallel assignment.  `spare'
    ;; is a register no move reads or writes.  `pending' mimics the Rust Vec
    ;; including `swap_remove'.
    (define (parallel-moves moves spare)
      (define pending (make-vector (length moves) #f))
      (define np 0)
      (define constants '())
      (for-each
        (lambda (m)
          (let ([s (car m)] [dst (cdr m)])
            (cond
              [(eq? (car s) 'reg)
               (unless (eqv? (cdr s) dst)
                 (vector-set! pending np (cons (cdr s) dst))
                 (set! np (+ np 1)))]
              [else (set! constants (cons (cons (cdr s) dst) constants))])))
        moves)
      (let loop ()
        (when (> np 0)
          (let ([ready
                 (let find ([i 0])
                   (cond
                     [(= i np) #f]
                     [(let ([dst (cdr (vector-ref pending i))])
                        (let any ([j 0])
                          (cond
                            [(= j np) #f]
                            [(eqv? (car (vector-ref pending j)) dst) #t]
                            [else (any (+ j 1))])))
                      (find (+ i 1))]
                     [else i]))])
            (cond
              [ready
               (let ([m (vector-ref pending ready)])
                 ;; swap_remove
                 (vector-set! pending ready (vector-ref pending (- np 1)))
                 (set! np (- np 1))
                 (mov (cdr m) (car m)))]
              [else
               ;; Every destination is still read: a cycle.
               (let ([dst (cdr (vector-ref pending 0))])
                 (mov spare dst)
                 (bump-extent! (+ spare 1))
                 (let redirect ([i 0])
                   (when (< i np)
                     (let ([m (vector-ref pending i)])
                       (when (eqv? (car m) dst)
                         (vector-set! pending i (cons spare (cdr m)))))
                     (redirect (+ i 1)))))])
            (loop))))
      (for-each (lambda (c) (load-const (cdr c) (car c))) (reverse constants)))

    ;; The words of the register mask of the uvars LIVE (registers from 2 up), up to
    ;; the last word with a bit set.
    (define (mask live)
      (let ([maxw (fold-left
                    (lambda (m v)
                      (let ([r (hashtable-ref registers v #f)])
                        (if (and r (>= r 2)) (max m (quotient r 32)) m)))
                    -1 live)])
        (if (< maxw 0)
          '()
          (let ([vec (make-vector (+ maxw 1) 0)])
            (for-each
              (lambda (v)
                (let ([r (hashtable-ref registers v #f)])
                  (when (and r (>= r 2))
                    (let ([w (quotient r 32)])
                      (vector-set! vec w
                        (bitwise-ior (vector-ref vec w)
                          (bitwise-arithmetic-shift-left 1 (remainder r 32))))))))
              live)
            (vector->list vec)))))

    ;; Pool index of a liveness mask, shared with identical masks.
    ;; The live sets are mostly the lists of `live-in`, shared by every use, so the
    ;; mask and the call base of one are computed once.
    (define mask-refs (make-eq-hashtable))
    (define (mask-ref live)
      (or (hashtable-ref mask-refs live #f)
        (let ([index (mask-ref* live)])
          (hashtable-set! mask-refs live index)
          index)))
    (define (mask-ref* live)
      (let ([bits (mask live)])
        (or (hashtable-ref (arena-mask-index arena) bits #f)
          (let* ([pool (arena-pool arena)]
                 [index (gvec-length pool)])
            (gvec-push! pool (length bits))
            (for-each (lambda (w) (gvec-push! pool w)) bits)
            (hashtable-set! (arena-mask-index arena) bits index)
            index))))

    (define (live-after id index)
      (live-after-index blocks live-in id index))

    (define call-bases (make-eq-hashtable))
    (define (call-base live)
      (or (hashtable-ref call-bases live #f)
        (let ([base (call-base* live)])
          (hashtable-set! call-bases live base)
          base)))
    (define (call-base* live)
      (+ 2
        (fold-left
          (lambda (m v)
            (let ([r (hashtable-ref registers v #f)])
              (if r (max m r) m)))
          1
          live)))

    (define (spare-above top) (max top (+ tmp-base max-tmp)))

    (define (cnt)
      (or cnt-register
        (assertion-violation 'lower
          "rest instruction in a procedure without a rest argument")))

    (define (set-undefined! dst-var)
      (when (used? dst-var)
        (let ([dst (reg dst-var)])
          (ins (list 'load-raw dst 10)))))               ; Value::undefined()

    (define (emit-add-immediate dst o value)
      (let ([src (operand o 0)])
        (ins (list 'add-immediate dst src value))))

    (define (emit-generic-prim dst prim args)
      (let ([primitive (prim-id prim)] [n (length args)])
        (if (<= n 3)
          (let ([registers
                 (let loop ([as args] [slot 0] [acc '()])
                   (if (null? as)
                     (reverse acc)
                     (let ([r (operand (car as) slot)])
                       (loop (cdr as) (+ slot 1) (cons r acc)))))])
            (ins (case n
                   [(0) (list 'call-primitive0 dst primitive)]
                   [(1) (list 'call-primitive1 dst primitive (car registers))]
                   [(2) (list 'call-primitive2 dst primitive (car registers) (cadr registers))]
                   [else (list 'call-primitive3 dst primitive (car registers)
                           (cadr registers) (caddr registers))])))
          ;; `primitive_n' reads contiguous registers.
          (let ([first (temp 0)])
            (let loop ([slot 1])
              (when (< slot n) (temp slot) (loop (+ slot 1))))
            (let* ([moves (let loop ([as args] [i 0] [acc '()])
                            (if (null? as)
                              (reverse acc)
                              (loop (cdr as) (+ i 1)
                                (cons (cons (src-of (car as)) (+ first i)) acc))))]
                   [spare (temp n)])
              (parallel-moves moves spare)
              (ins (list 'call-primitive-n dst primitive first n)))))))

    ;; Constant fixnum value of operand `o' used at instruction `index' of
    ;; `id', following const / assign chains.
    (define (const-fixnum id index o)
      (cond
        [(and (operand-constant? o) (fixnum? (operand-constant-value o)))
         (operand-constant-value o)]
        [(operand-local? o)
         (let ([insns (insn-vector id)])
           (let loop ([current (operand-local-uvar o)] [end index])
             (let ([pos (let find ([i (- end 1)])
                          (cond
                            [(< i 0) #f]
                            [(eqv? (insn-def (vector-ref insns i)) current) i]
                            [else (find (- i 1))]))])
               (if (not pos)
                 #f
                 (let ([insn (vector-ref insns pos)])
                   (cond
                     [(insn-const? insn)
                      (let ([v (insn-const-value insn)]) (and (fixnum? v) v))]
                     [(insn-assign? insn)
                      (let ([s (insn-assign-src insn)])
                        (if (operand-constant? s)
                          (let ([v (operand-constant-value s)]) (and (fixnum? v) v))
                          (loop (operand-local-uvar s) pos)))]
                     [else #f]))))))]
        [else #f]))

    ;; A call to a runtime procedure with `args' (Src list) in the frame at
    ;; `base`, received with `mv' value semantics into `dst'.
    (define (emit-runtime-call proc args base mask-index mv dst)
      (let* ([moves (let loop ([as args] [i 0] [acc '()])
                      (if (null? as)
                        (reverse acc)
                        (loop (cdr as) (+ i 1)
                          (cons (cons (car as) (+ base 2 i)) acc))))]
             [top (+ base 2 (length args))]
             [spare (spare-above top)])
        (bump-extent! top)
        (parallel-moves moves spare)
        (ins (list 'runtime-procedure (+ base 1) (runtime-proc-id proc)))
        (ins (list 'call (length args) base))
        (ins (list 'return-point base mask-index mv dst))))

    (define (emit-slot-slow-inline id index dst-var prim args)
      (let* ([live (uset-remove (live-after id index) dst-var)]
             [base (call-base live)]
             [mask (mask-ref live)]
             [proc (if (eq? prim 'slot-set!) 'slot-set! 'slot-ref)]
             [operands (map src-of args)])
        (cond
          [(eq? prim 'slot-set!)
           (emit-runtime-call proc operands base mask mv-ignore (+ base 2))
           (set-undefined! dst-var)]
          [(used? dst-var)
           (let ([dst (reg dst-var)])
             (emit-runtime-call proc operands base mask mv-error dst))]
          [else
           (emit-runtime-call proc operands base mask mv-ignore (+ base 2))])))

    (define (symbol-constant-operand? o)
      (and (operand-constant? o) (symbol? (operand-constant-value o))))

    (define (emit-slot-ref id index dst-var args)
      (if (not (and (= (length args) 2) (symbol-constant-operand? (cadr args))))
        (emit-slot-slow-inline id index dst-var 'slot-ref args)
        (let* ([name (operand-constant-value (cadr args))]
               [dst (reg dst-var)]
               [obj (operand (car args) 0)]
               [name-const (value-constant name)]
               [label-slow (new-label)]
               [label-next (new-label)])
          (branch! (list 'get-slot dst obj name-const label-slow))
          (push-label! label-next)
          (let* ([live (uset-remove (live-after id index) dst-var)]
                 [base (call-base live)]
                 [mask (mask-ref live)])
            (set! slot-stubs
              (cons (vector label-slow label-next #t dst-var dst obj name 0 base mask)
                slot-stubs))))))

    (define (emit-slot-set id index dst-var args)
      (if (not (and (= (length args) 3) (symbol-constant-operand? (cadr args))))
        (emit-slot-slow-inline id index dst-var 'slot-set! args)
        (let* ([name (operand-constant-value (cadr args))]
               [obj (operand (car args) 0)]
               [src (operand (caddr args) 1)]
               [name-const (value-constant name)]
               [label-slow (new-label)]
               [label-next (new-label)])
          (branch! (list 'put-slot obj name-const src label-slow))
          (push-label! label-next)
          ;; Both the fast path and the stub (which jumps here) produce no value.
          (set-undefined! dst-var)
          (let* ([live (uset-remove (live-after id index) dst-var)]
                 [base (call-base live)]
                 [mask (mask-ref live)])
            (set! slot-stubs
              (cons (vector label-slow label-next #f dst-var 0 obj name src base mask)
                slot-stubs))))))

    ;; The cold section.
    (define (emit-slot-stubs)
      (note-source! #f)
      (for-each
        (lambda (stub)
          (let ([label-slow (vector-ref stub 0)] [label-next (vector-ref stub 1)]
                [get? (vector-ref stub 2)] [dst-var (vector-ref stub 3)]
                [dst (vector-ref stub 4)] [obj (vector-ref stub 5)]
                [name (vector-ref stub 6)] [src (vector-ref stub 7)]
                [base (vector-ref stub 8)] [mask (vector-ref stub 9)])
            (push-label! label-slow)
            (if get?
              (let-values ([(mv d) (if (used? dst-var)
                                     (values mv-error dst)
                                     (values mv-ignore (+ base 2)))])
                (emit-runtime-call 'slot-ref
                  (list (cons 'reg obj) (cons 'const name))
                  base mask mv d))
              (emit-runtime-call 'slot-set!
                (list (cons 'reg obj) (cons 'const name) (cons 'reg src))
                base mask mv-ignore (+ base 2)))
            (branch! (list 'jump label-next))))
        (reverse slot-stubs))
      (set! slot-stubs '()))

    (define (emit-cmp op dst args)
      (let* ([lhs (operand (car args) 0)]
             [rhs (operand (cadr args) 1)])
        (ins (list op dst lhs rhs))))

    (define (emit-binary op dst args)
      (emit-cmp op dst args))

    (define (emit-unary op dst args)
      (let ([src (operand (car args) 0)])
        (ins (list op dst src))))

    (define (emit-prim id index dst-var prim args)
      (define dst (reg dst-var))
      (define n (length args))
      (define (is? p k) (and (eq? prim p) (= n k)))
      (cond
        [(is? 'car 1) (emit-unary 'car dst args)]
        [(is? 'cdr 1) (emit-unary 'cdr dst args)]
        [(is? 'cons 2) (emit-binary 'cons dst args)]
        [(is? 'set-car! 2)
         (let* ([pair (operand (car args) 0)]
                [src (operand (cadr args) 1)])
           (ins (list 'set-car pair src))
           (set-undefined! dst-var))]
        [(is? 'set-cdr! 2)
         (let* ([pair (operand (car args) 0)]
                [src (operand (cadr args) 1)])
           (ins (list 'set-cdr pair src))
           (set-undefined! dst-var))]
        [(is? 'eq? 2) (emit-binary 'eq dst args)]
        [(is? 'not 1) (emit-unary 'not dst args)]
        [(is? '+ 2)
         (let* ([c0 (const-fixnum id index (car args))]
                [c1 (const-fixnum id index (cadr args))])
           (cond
             [(and (not c0) c1) (emit-add-immediate dst (car args) c1)]
             [(and c0 (not c1)) (emit-add-immediate dst (cadr args) c0)]
             [else (emit-binary 'add dst args)]))]
        [(is? '- 2)
         (let* ([c (const-fixnum id index (cadr args))]
                ;; checked_neg: i32::MIN has no negation
                [negated (and c (not (= c -2147483648)) (- c))])
           (if negated
             (emit-add-immediate dst (car args) negated)
             (emit-binary 'sub dst args)))]
        [(is? '* 2) (emit-binary 'mul dst args)]
        [(is? '/ 2) (emit-binary 'div dst args)]
        [(is? 'quotient 2) (emit-binary 'quotient dst args)]
        [(is? 'remainder 2) (emit-binary 'remainder dst args)]
        [(is? 'modulo 2) (emit-binary 'modulo dst args)]
        [(is? '- 1) (emit-unary 'negate dst args)]
        [(is? 'abs 1) (emit-unary 'abs dst args)]
        [(is? 'sqrt 1) (emit-unary 'sqrt dst args)]
        [(is? '< 2) (emit-cmp 'less dst args)]
        [(is? '<= 2) (emit-cmp 'less-equal dst args)]
        [(is? '= 2) (emit-cmp 'number-equal dst args)]
        [(is? '>= 2) (emit-cmp 'greater-equal dst args)]
        [(is? '> 2) (emit-cmp 'greater dst args)]
        [(is? 'vector-ref 2) (emit-binary 'vector-ref dst args)]
        [(is? 'vector-length 1) (emit-unary 'vector-length dst args)]
        [(is? 'vector-set! 3)
         (let* ([vector (operand (car args) 0)]
                [idx (operand (cadr args) 1)]
                [src (operand (caddr args) 2)])
           (ins (list 'vector-set vector idx src))
           (set-undefined! dst-var))]
        [(is? 'variable-ref 1) (emit-unary 'variable-ref dst args)]
        [(is? 'variable-set! 2)
         (let* ([var (operand (car args) 0)]
                [src (operand (cadr args) 1)])
           (ins (list 'variable-set var src))
           (set-undefined! dst-var))]
        [(or (eq? prim 'cache-ref) (eq? prim 'cache-set!))
         (assertion-violation 'lower "primitive reached bytecode lowering unlowered" prim)]
        [(is? 'slot-ref 2) (emit-slot-ref id index dst-var args)]
        [(is? 'slot-set! 3) (emit-slot-set id index dst-var args)]
        ;; Malformed by the time lowering sees it: call the real procedure
        ;; inline, which raises the arity error itself.
        [(or (eq? prim 'slot-ref) (eq? prim 'slot-set!))
         (emit-slot-slow-inline id index dst-var prim args)]
        [else (emit-generic-prim dst prim args)]))

    (define (emit-instruction id index insn)
      (cond
        [(insn-assign? insn)
         (let ([dst (reg (insn-assign-dst insn))] [s (insn-assign-src insn)])
           (if (operand-local? s)
             (mov dst (reg (operand-local-uvar s)))
             (load-const dst (operand-constant-value s))))]
        [(insn-const? insn)
         (let ([dst (reg (insn-const-dst insn))])
           (load-const dst (insn-const-value insn)))]
        [(insn-make-closure? insn)
         (let* ([dst (reg (insn-make-closure-dst insn))]
                [idx (code-constant (insn-make-closure-code insn))]
                [env (insn-make-closure-env insn)]
                [nfree (insn-make-closure-free-count insn)])
           (if env
             (let ([e (operand env 0)])
               (ins (list 'closure-env dst idx nfree e)))
             (ins (list 'closure dst idx nfree))))]
        [(insn-make-case-closure? insn)
         (let* ([dst (reg (insn-make-case-closure-dst insn))]
                [idx (code-constant (insn-make-case-closure-code insn))]
                [clauses (insn-make-case-closure-clauses insn)])
           (ins (list 'closure dst idx (length clauses)))
           (let loop ([cs clauses] [slot 0])
             (unless (null? cs)
               (let ([src (operand (car cs) (+ slot 1))])
                 (ins (list 'closure-set dst slot src)))
               (loop (cdr cs) (+ slot 1)))))]
        [(insn-make-env? insn)
         (let ([dst (reg (insn-make-env-dst insn))])
           (ins (list 'make-env dst (insn-make-env-size insn))))]
        [(insn-env-ref? insn)
         (let* ([env (operand (insn-env-ref-env insn) 0)]
                [dst (reg (insn-env-ref-dst insn))])
           (ins (list 'env-ref dst env (insn-env-ref-index insn))))]
        [(insn-env-set? insn)
         (let* ([env (operand (insn-env-set-env insn) 0)]
                [value (operand (insn-env-set-value insn) 1)])
           (ins (list 'env-set env (insn-env-set-index insn) value)))]
        [(insn-closure-ref? insn)
         (let* ([closure (insn-closure-ref-closure insn)]
                [index (insn-closure-ref-index insn)]
                [is-self (and (operand-local? closure)
                              (eqv? (operand-local-uvar closure) binding))]
                [dst (reg (insn-closure-ref-dst insn))])
           (if is-self
             ;; The self reference has no slot: it is the closure in r1.
             (let ([self-reference
                    (and (or (not (= index 0)) (not (cfg-proc-env-shared? procedure)))
                         (let ([fv (cfg-proc-free-vars procedure)])
                           (and (< index (length fv))
                                (let ([a (assv (list-ref fv index) (cfg-proc-sources procedure))]
                                      [b (assv binding (cfg-proc-sources procedure))])
                                  (and a b (eq? (cdr a) (cdr b)))))))])
               (if self-reference
                 (mov dst 1)
                 (ins (list 'free-ref dst index))))
             (let ([c (operand closure 0)])
               (ins (list 'closure-ref dst c index)))))]
        [(insn-closure-set? insn)
         (let* ([closure (operand (insn-closure-set-closure insn) 0)]
                [value (operand (insn-closure-set-value insn) 1)])
           (ins (list 'closure-set closure (insn-closure-set-index insn) value)))]
        [(insn-cache-ref? insn)
         (note-source! (insn-cache-ref-source insn))
         (let ([key (insn-cache-ref-cache-key insn)])
           (unless (operand-constant? key)
             (assertion-violation 'lower "cache-ref expects a constant key" key))
           (let* ([idx (cell-constant (operand-constant-value key))]
                  [dst (reg (insn-cache-ref-dst insn))])
             (ins (list 'cell-ref dst idx))))]
        [(insn-cache-set? insn)
         (note-source! (insn-cache-set-source insn))
         (let ([key (insn-cache-set-cache-key insn)])
           (unless (operand-constant? key)
             (assertion-violation 'lower "cache-set! expects a constant key" key))
           (let* ([idx (cell-constant (operand-constant-value key))]
                  [value (operand (insn-cache-set-value insn) 0)])
             (ins (list 'cell-set idx value))
             (set-undefined! (insn-cache-set-dst insn))))]
        [(insn-prim-call? insn)
         (note-source! (insn-prim-call-source insn))
         (emit-prim id index (insn-prim-call-dst insn) (insn-prim-call-prim insn)
           (insn-prim-call-args insn))]
        [(insn-rest-to-list? insn)
         (note-source! (insn-rest-to-list-source insn))
         (let* ([c (cnt)]
                [dst (reg (insn-rest-to-list-dst insn))])
           (set! rest-fixups (cons (gvec-length atoms) rest-fixups))
           (ins (list 'rest-list dst c 0 0)))]
        [(insn-rest-ref? insn)
         (note-source! (insn-rest-ref-source insn))
         (let* ([c (cnt)]
                [dst (reg (insn-rest-ref-dst insn))])
           (set! rest-fixups (cons (gvec-length atoms) rest-fixups))
           (ins (list 'rest-ref dst c 0 (insn-rest-ref-index insn))))]
        [(insn-rest-length? insn)
         (note-source! (insn-rest-length-source insn))
         (let* ([c (cnt)]
                [dst (reg (insn-rest-length-dst insn))])
           (ins (list 'rest-len dst c (insn-rest-length-skip insn))))]
        [(insn-rest-predicate? insn)
         (note-source! (insn-rest-predicate-source insn))
         (let ([dst (reg (insn-rest-predicate-dst insn))]
               [predicate (insn-rest-predicate-predicate insn)]
               [skip (insn-rest-predicate-skip insn)])
           (case predicate
             [(List) (load-const dst #t)]
             [(Null) (let ([c (cnt)]) (ins (list 'rest-null dst c skip)))]
             [else (let ([c (cnt)]) (ins (list 'rest-pair dst c skip)))]))]
        ;; Bound by the moves after the return point of each call that
        ;; reaches this block.
        [else (unspecific)]))

    (define (goto-block block next)
      (unless (eqv? block next)
        (branch! (list 'jump (block-label block)))))

    (define (goto target next)
      (if (target-local? target)
        (goto-block (target-local-block target) next)
        (emit-return (target-return-args target))))

    (define (emit-return args)
      (cond
        [(and (= (length args) 1) (operand-local? (car args)))
         (let ([register (reg (operand-local-uvar (car args)))])
           (bump-extent! 3)
           (ins (list 'return register)))]
        [else
         (let* ([moves (let loop ([as args] [i 0] [acc '()])
                         (if (null? as)
                           (reverse acc)
                           (loop (cdr as) (+ i 1)
                             (cons (cons (src-of (car as)) (+ 2 i)) acc))))]
                [top (+ 2 (length args))])
           (bump-extent! top)
           (let ([spare (spare-above top)])
             (parallel-moves moves spare)
             (if (= (length args) 1)
               (ins (list 'return 2))
               (ins (list 'return-values (length args))))))]))

    ;; A direct self call: the callee is the binding (r1, never reassigned) and
    ;; the argument count is accepted by the shared `enter'.
    (define (direct-self-call? callee argc)
      (and (operand-local? callee)
        (eqv? (operand-local-uvar callee) binding)
        (if variadic? (>= argc nparams) (= argc nparams))))

    (define (emit-call callee args ret next)
      ;; `(apply f a ... lst)': the operands are `f a ... lst'.
      (let* ([applying (apply-call-callee? callee)]
             [callee (if applying (car args) callee)]
             [args (if applying (cdr args) args)])
        (emit-call* callee args ret next applying)))

    (define (emit-call* callee args ret next applying)
      (let* ([ret-block (return-point-block ret)]
             [live (hashtable-ref live-in ret-block '())]
             [base (call-base live)]
             [direct (and (not applying) (direct-self-call? callee (length args)))]
             [moves (append
                      (if direct '() (list (cons (src-of callee) (+ base 1))))
                      (let loop ([as args] [i 0] [acc '()])
                        (if (null? as)
                          (reverse acc)
                          (loop (cdr as) (+ i 1)
                            (cons (cons (src-of (car as)) (+ base 2 i)) acc)))))]
             [top (+ base 2 (length args))]
             [spare (spare-above top)])
        (bump-extent! top)
        (parallel-moves moves spare)
        (ins (cond
               [direct (list 'call-self (length args) base)]
               [applying (list 'apply (length args) base)]
               [else (list 'call (length args) base)]))
        (let* ([block (block-of ret-block)]
               [insns (block-instructions block)]
               [rest-used (exists (lambda (i)
                                    (and (insn-receive-rest? i) (used? (insn-receive-rest-dst i))))
                            insns)]
               [any-used (or rest-used
                             (exists (lambda (i)
                                       (and (insn-receive? i) (used? (insn-receive-dst i))))
                               insns))]
               [mask (mask-ref live)]
               [nvals (return-point-nvals ret)]
               [rvariadic (return-point-variadic? ret)]
               [single (or (and (= nvals 1) (not rvariadic))
                           (and rvariadic (= nvals 0) (not any-used)))]
               ;; The single value lands directly in the destination of the
               ;; first used `Receive' of value 0.  A dropped value lands in
               ;; r[base+2], where it arrives anyway.
               [bound (let ([found
                             (and single
                                  (let find ([is insns])
                                    (cond
                                      [(null? is) #f]
                                      [(and (insn-receive? (car is))
                                            (eqv? (insn-receive-index (car is)) 0)
                                            (used? (insn-receive-dst (car is))))
                                       (insn-receive-dst (car is))]
                                      [else (find (cdr is))])))])
                        (if found (reg found) (+ base 2)))])
          (cond
            [single
             (let ([mv (if any-used mv-error mv-ignore)])
               (ins (list 'return-point base mask mv bound)))]
            [else
             ;; Several values, none, or a rest list: one value enters the
             ;; multiple-value entry right after the header with a count of 1.
             (ins (list 'return-point-forward base mask))
             (ins (if rvariadic
                    (list 'return-point-values-rest nvals base)
                    (list 'return-point-values nvals base)))])
          ;; Bind the returned values.  Several values sit where the arguments
          ;; were, a single value is already in its destination.
          (let ([receives '()]             ; newest first
                [highest (+ base 2)]
                [skipped (not single)])
            (for-each
              (lambda (i)
                (cond
                  [(insn-receive? i)
                   (let* ([dst (insn-receive-dst i)]
                          [index (insn-receive-index i)]
                          [source (+ base 2 index)])
                     (set! highest (max highest (+ source 1)))
                     (when (used? dst)
                       (if (and single (eqv? index 0) (not skipped))
                         (set! skipped #t)
                         (set! receives
                           (cons (cons (cons 'reg source) (reg dst)) receives)))))]
                  ;; Extra values nobody looks at: the return point has already
                  ;; checked the count, and there is no list to build.
                  [(and (insn-receive-rest? i) (not (used? (insn-receive-rest-dst i)))) (unspecific)]
                  [(insn-receive-rest? i)
                   ;; The list replaces the count in the slot at `base'.
                   (ins (list 'values-rest base (insn-receive-rest-from i)))
                   (set! receives
                     (cons (cons (cons 'reg base) (reg (insn-receive-rest-dst i))) receives))]
                  [else (unspecific)]))
              insns)
            (bump-extent! highest)
            (let ([spare (spare-above (max highest top))])
              (parallel-moves (reverse receives) spare))
            (goto-block ret-block next)))))

    (define (emit-tail-call callee args)
      (let* ([applying (apply-call-callee? callee)]
             [callee (if applying (car args) callee)]
             [args (if applying (cdr args) args)]
             [direct (and (not applying) (direct-self-call? callee (length args)))]
             [moves (append
                      (if direct '() (list (cons (src-of callee) 1)))
                      (let loop ([as args] [i 0] [acc '()])
                        (if (null? as)
                          (reverse acc)
                          (loop (cdr as) (+ i 1)
                            (cons (cons (src-of (car as)) (+ 2 i)) acc)))))]
             [top (+ 2 (length args))])
        (bump-extent! top)
        (let ([spare (spare-above top)])
          (parallel-moves moves spare)
          (ins (cond
                 [direct (list 'tail-call-self (length args))]
                 [applying (list 'tail-apply (length args))]
                 [else (list 'tail-call (length args))])))))

    ;; Emit a jump to `label' taken when `cond' is `sense'.
    (define (jump-if cond-spec sense label)
      (case (car cond-spec)
        [(truthy falsy)
         (let ([truthy (eq? (eq? (car cond-spec) 'truthy) sense)])
           (branch! (list (if truthy 'jump-if-true 'jump-if-false) (cadr cond-spec) label)))]
        [(type)
         (let ([kind (if sense (cadr cond-spec) (caddr cond-spec))])
           (branch! (list (sym-op kind) (cadddr cond-spec) label)))]
        [else
         (let ([kind (if sense (cadr cond-spec) (caddr cond-spec))])
           (branch! (list (cmp-op kind) (cadddr cond-spec) (list-ref cond-spec 4) label)))]))

    (define (emit-branch cond-spec consequent alternative next)
      (cond
        [(and (target-local? consequent) (target-local? alternative))
         (let ([yes (target-local-block consequent)] [no (target-local-block alternative)])
           (if (eqv? yes next)
             (jump-if cond-spec #f (block-label no))
             (begin
               (jump-if cond-spec #t (block-label yes))
               (goto-block no next))))]
        [(target-local? consequent)
         (jump-if cond-spec #t (block-label (target-local-block consequent)))
         (emit-return (target-return-args alternative))]
        [(target-local? alternative)
         (jump-if cond-spec #f (block-label (target-local-block alternative)))
         (emit-return (target-return-args consequent))]
        [else
         (let ([label (new-label)])
           (jump-if cond-spec #f label)
           (emit-return (target-return-args consequent))
           (push-label! label)
           (emit-return (target-return-args alternative)))]))

    ;; The test a branch on `prim(args)' jumps on, fused where an instruction
    ;; exists and computed into a temporary otherwise.
    (define (prim-cond prim args)
      (let* ([n (length args)]
             [kinds (fused-jump-kinds prim n)])
        (cond
          [kinds
           (if (= n 1)
             (let ([r (operand (car args) 0)])
               (list 'type (car kinds) (cdr kinds) r))
             (let* ([a (operand (car args) 0)]
                    [b (operand (cadr args) 1)])
               (list 'cmp (car kinds) (cdr kinds) a b)))]
          [(and (eq? prim 'not) (= n 1))
           (list 'falsy (operand (car args) 0))]
          [else
           (let ([result (temp n)])
             (emit-generic-prim result prim args)
             (list 'truthy result))])))

    ;; fusion: #(prim args sense elided)
    (define (fused-cond fusion)
      (let* ([prim (vector-ref fusion 0)]
             [args (vector-ref fusion 1)]
             [kinds (fused-jump-kinds prim (length args))]
             [yes (if (vector-ref fusion 2) (car kinds) (cdr kinds))]
             [no (if (vector-ref fusion 2) (cdr kinds) (car kinds))])
        (if (= (length args) 1)
          (let ([r (operand (car args) 0)])
            (list 'type yes no r))
          (let* ([a (operand (car args) 0)]
                 [b (operand (cadr args) 1)])
            (list 'cmp yes no a b)))))

    (define (find-def-before insns end var)
      (let find ([i (- end 1)])
        (cond
          [(< i 0) #f]
          [(eqv? (insn-def (vector-ref insns i)) var) i]
          [else (find (- i 1))])))

    ;; -> (prim args sense links) with links = ((pos . var) ...) in push order
    (define (resolve-branch-test id operand sense)
      (let* ([insns (insn-vector id)]
             [branch (vector-length insns)])
        (let loop ([current operand] [csense sense] [end branch] [links '()])
          (if (not (operand-local? current))
            #f
            (let* ([var (operand-local-uvar current)]
                   [pos (find-def-before insns end var)])
              (if (not pos)
                #f
                (let ([insn (vector-ref insns pos)])
                  (cond
                    [(insn-prim-call? insn)
                     (let ([prim (insn-prim-call-prim insn)]
                           [args (insn-prim-call-args insn)])
                       (cond
                         [(and (eq? prim 'not) (= (length args) 1))
                          (loop (car args) (not csense) pos (cons (cons pos var) links))]
                         [(not (fused-jump-kinds prim (length args))) #f]
                         [(let rebound ([j pos])
                            (and (< j branch)
                                 (let ([d (insn-def (vector-ref insns j))])
                                   (or (and d
                                            (exists (lambda (a)
                                                      (and (operand-local? a)
                                                           (eqv? (operand-local-uvar a) d)))
                                              args))
                                       (rebound (+ j 1))))))
                          #f]
                         [else (list prim args csense (reverse (cons (cons pos var) links)))]))]
                    [(and (insn-assign? insn) (operand-local? (insn-assign-src insn)))
                     (loop (insn-assign-src insn) csense pos (cons (cons pos var) links))]
                    [else #f]))))))))

    ;; Whether `var' has exactly one syntactic use in the procedure.
    (define (single-use? var)
      (let ([count 0])
        (for-each
          (lambda (b)
            (for-each
              (lambda (i)
                (for-each (lambda (u) (when (eqv? u var) (set! count (+ count 1))))
                  (insn-use-vars i)))
              (block-instructions b))
            (for-each (lambda (u) (when (eqv? u var) (set! count (+ count 1))))
              (term-use-vars (block-terminator b))))
          (cfg-proc-blocks procedure))
        (= count 1)))

    (define (plan-branch-fusion id term)
      (let-values ([(operand sense)
                    (cond
                      [(term-branch? term) (values (term-branch-test term) #t)]
                      [(and (term-branch-prim? term)
                            (eq? (term-branch-prim-prim term) 'not)
                            (= (length (term-branch-prim-args term)) 1))
                       (values (car (term-branch-prim-args term)) #f)]
                      [else (values #f #f)])])
        (and operand
          (let ([r (resolve-branch-test id operand sense)])
            (and r
              (let* ([prim (car r)] [args (cadr r)] [sense (caddr r)] [links (cadddr r)]
                     [positions (map car links)]
                     [n (vector-length (insn-vector id))]
                     [elided '()]
                     [reader-gone #t])
                (for-each
                  (lambda (link)
                    (let ([clean (let check ([k (+ (car link) 1)])
                                   (or (>= k n)
                                       (and (memv k positions) (check (+ k 1)))))])
                      (if (and reader-gone clean (single-use? (cdr link)))
                        (set! elided (cons (car link) elided))
                        (set! reader-gone #f))))
                  links)
                (vector prim args sense (reverse elided))))))))

    ;; Label of a branch target; a `Return' target gets a stub, queued in
    ;; `stubs' (a box: list newest first) and emitted by the caller.
    (define (target-label target stubs)
      (if (target-local? target)
        (block-label (target-local-block target))
        (let ([label (new-label)])
          (set-car! stubs (cons (cons label (target-return-args target)) (car stubs)))
          label)))

    (define (emit-switch kind scrutinee cases default)
      (let* ([stubs (list '())]
             [default-label (target-label default stubs)]
             [scrutinee-register (operand scrutinee 0)])
        (cond
          [(pair? kind)                      ; (SymbolEq . mask)
           (let ([mask (cdr kind)]
                 [buckets '()])              ; newest first: #(key label members-newest-first)
             ;; Symbols with the same masked hash share a bucket.
             (for-each
               (lambda (c)
                 (let ([v (switch-case-value c)])
                   (when (case-symbol? v)
                     (let* ([key (bitwise-and (case-symbol-hash v) mask)]
                            [target (target-label (switch-case-target c) stubs)]
                            [bucket (let find ([bs buckets])
                                      (cond [(null? bs) #f]
                                            [(= (vector-ref (car bs) 0) key) (car bs)]
                                            [else (find (cdr bs))]))])
                       (if bucket
                         (vector-set! bucket 2
                           (cons (cons (case-symbol-value v) target) (vector-ref bucket 2)))
                         (set! buckets
                           (cons (vector key (new-label)
                                   (list (cons (case-symbol-value v) target)))
                             buckets)))))))
               cases)
             (let* ([sorted (list-sort (lambda (a b) (< (vector-ref a 0) (vector-ref b 0)))
                              (reverse buckets))]
                    [atom (gvec-length atoms)])
               (ins (list 'switch-symbol scrutinee-register 0))
               (set! switch-fixups
                 (cons (list 'sym atom mask default-label
                         (map (lambda (b) (cons (vector-ref b 0) (vector-ref b 1))) sorted))
                   switch-fixups))
               ;; Not a symbol.
               (branch! (list 'jump default-label))
               (for-each
                 (lambda (b)
                   (push-label! (vector-ref b 1))
                   (for-each
                     (lambda (member)
                       (let ([constant (temp 1)])
                         (load-const constant (car member))
                         (branch! (list 'jump-if-eq scrutinee-register constant (cdr member)))))
                     (reverse (vector-ref b 2)))
                   (branch! (list 'jump default-label)))
                 sorted)))]
          [else
           ;; The first case with a given key wins.
           (let ([table '()]                 ; newest first (key . label)
                 [integer-cases '()])        ; newest first
             (for-each
               (lambda (c)
                 (let ([v (switch-case-value c)])
                   (when (case-integer? v)
                     (let ([key (case-integer-value v)]
                           [target (target-label (switch-case-target c) stubs)])
                       (set! integer-cases (cons (cons key target) integer-cases))
                       (unless (assv key table)
                         (set! table (cons (cons key target) table)))))))
               cases)
             (let* ([integer-cases (reverse integer-cases)]
                    [table (list-sort (lambda (a b) (< (car a) (car b))) (reverse table))]
                    [atom (gvec-length atoms)])
               (if (memq kind '(Char CharEq))
                 (ins (list 'switch-char scrutinee-register 0))
                 (ins (list 'switch-fixnum scrutinee-register 0)))
               (set! switch-fixups
                 (cons (list 'int atom default-label table) switch-fixups))
               ;; The scrutinee has another type.
               (case kind
                 [(Fixnum)
                  (when (pair? integer-cases)
                    ;; Raises for a non-fixnum, as `fx=?' would.
                    (let ([result (temp 2)])
                      (emit-generic-prim result 'fx=?
                        (list scrutinee (make-operand-constant (car (car integer-cases)))))))]
                 [(Numeric)
                  (for-each
                    (lambda (c)
                      (let ([result (temp 2)])
                        (emit-generic-prim result '=
                          (list scrutinee (make-operand-constant (car c))))
                        (jump-if (list 'truthy result) #t (cdr c))))
                    integer-cases)]
                 [(Char)
                  ;; Raises for a non-char, as `char->integer' would.
                  (let ([result (temp 2)])
                    (emit-generic-prim result 'char->integer (list scrutinee)))]
                 [else (unspecific)])
               (branch! (list 'jump default-label))))])
        (for-each
          (lambda (stub)
            (push-label! (car stub))
            (emit-return (cdr stub)))
          (reverse (car stubs)))))

    (define (emit-terminator term fusion next)
      (cond
        [(term-jump? term) (goto-block (term-jump-target term) next)]
        [(term-return? term)
         (note-source! (term-return-source term))
         (emit-return (term-return-args term))]
        [(term-tail-call? term)
         (note-source! (term-tail-call-source term))
         (emit-tail-call (term-tail-call-callee term) (term-tail-call-args term))]
        [(term-call? term)
         (note-source! (term-call-source term))
         (emit-call (term-call-callee term) (term-call-args term) (term-call-ret term) next)]
        [(term-raise? term)
         (note-source! (term-raise-source term))
         (let* ([args (term-raise-args term)]
                [moves (let loop ([as args] [i 0] [acc '()])
                         (if (null? as)
                           (reverse acc)
                           (loop (cdr as) (+ i 1)
                             (cons (cons (src-of (car as)) (+ 2 i)) acc))))]
                [top (+ 2 (length args))])
           (bump-extent! top)
           (let ([spare (spare-above top)])
             (parallel-moves moves spare)
             (ins (list 'raise (raise-kind->index (term-raise-kind term)) (length args)))))]
        [(term-branch? term)
         (let ([test (term-branch-test term)]
               [consequent (term-branch-consequent term)]
               [alternative (term-branch-alternative term)])
           (cond
             [fusion (emit-branch (fused-cond fusion) consequent alternative next)]
             [(operand-constant? test)
              ;; A constant test is a jump.
              (goto (if (operand-constant-value test) consequent alternative) next)]
             [else
              (emit-branch (list 'truthy (reg (operand-local-uvar test)))
                consequent alternative next)]))]
        [(term-branch-prim? term)
         (let ([cond-spec (if fusion
                            (fused-cond fusion)
                            (prim-cond (term-branch-prim-prim term) (term-branch-prim-args term)))])
           (emit-branch cond-spec (term-branch-prim-consequent term)
             (term-branch-prim-alternative term) next))]
        [else
         (emit-switch (term-switch-kind term) (term-switch-scrutinee term)
           (term-switch-cases term) (term-switch-default term))]))

    (define (emit-procedure)
      (note-source! #f)
      (ins (list 'nop))
      (bump-extent! tmp-base)
      (let loop ([ids order])
        (unless (null? ids)
          (let* ([id (car ids)]
                 [next (if (pair? (cdr ids)) (cadr ids) #f)]
                 [block (block-of id)]
                 [label (block-label id)])
            (push-label! label)
            (when (hashtable-ref loop-headers id #f)
              (let ([mask (mask-ref (hashtable-ref live-in id '()))])
                (ins (list 'loop-hint mask))))
            (note-source! (block-source block))
            (let* ([term (block-terminator block)]
                   [fusion (plan-branch-fusion id term)]
                   [elided (if fusion (vector-ref fusion 3) '())])
              (let iloop ([is (block-instructions block)] [index 0])
                (unless (null? is)
                  (unless (memv index elided)
                    (emit-instruction id index (car is)))
                  (iloop (cdr is) (+ index 1))))
              (emit-terminator term fusion next)))
          (loop (cdr ids))))
      (emit-slot-stubs)
      (let* ([rest-base (+ tmp-base max-tmp)])
        (bump-extent! rest-base)
        (let ([size (+ extent 1)])
          (when (>= size 65536)
            (assertion-violation 'lower "frame is too large" (cfg-proc-name procedure) size))
          (for-each
            (lambda (atom)
              (let ([e (gvec-ref atoms atom)])
                ;; rest-ref / rest-list: (op dst cnt base x)
                (gvec-set! atoms atom
                  (list (car e) (cadr e) (caddr e) rest-base (list-ref e 4)))))
            rest-fixups)
          (gvec-set! atoms 0
            (if cnt-register
              (list 'enter-rest nparams cnt-register rest-base size)
              (list 'enter nparams size))))))

    (emit-procedure)
    (let ([size (+ extent 1)])
      (make-emitted
        (gvec->list atoms)
        (reverse sources)
        (reverse switch-fixups)
        size
        (if variadic? (- (+ nparams 1)) nparams)
        (cfg-proc-name procedure)
        (cfg-proc-meta procedure)
        (cfg-proc-source procedure))))

  ;; =========================================================================
  ;; Linking: pool tables, encoding, srcmap
  ;; =========================================================================
  ;; Rust: `Lower::link_parts'.  `case-rows': #f or a list of
  ;; #(min max label frame rest-base rest-cnt) for a case-lambda dispatcher.
  (define (link-parts code-id atom-list sources switch-fixups arena name meta source
                      arity size case-rows)
    (define atoms (list->vector atom-list))
    (define pool (arena-pool arena))
    (define pending '())                    ; (slot atom label)
    (define (push-word! w) (gvec-push! pool w))
    (define (pool-length) (gvec-length pool))
    (define (patch-operand! atom pos value)
      (let ([e (vector-ref atoms atom)])
        (vector-set! atoms atom
          (let loop ([l e] [i 0])
            (cond
              [(null? l) '()]
              [(= i pos) (cons value (cdr l))]
              [else (cons (car l) (loop (cdr l) (+ i 1)))])))))
    (define (add-pending! slot atom label)
      (set! pending (cons (list slot atom label) pending)))
    (for-each
      (lambda (fixup)
        (case (car fixup)
          [(int)
           (let* ([atom (cadr fixup)] [default (caddr fixup)] [cases (cadddr fixup)]
                  [table (pool-length)])
             ;; operand 2 of switch-fixnum / switch-char: (op src table)
             (patch-operand! atom 2 table)
             ;; `cases' is sorted and deduplicated by key.
             (let ([dense (dense-span-for-keys (map car cases))])
               (add-pending! (pool-length) atom default)
               (push-word! 0)
               (cond
                 [dense
                  (let ([mn (car dense)] [span (cdr dense)])
                    (push-word! (bitwise-ior span switch-dense-flag))
                    (push-word! (bitwise-and mn u32-mask))
                    (let loop ([offset 0] [cs cases])
                      (when (< offset span)
                        (let ([key (+ mn offset)])
                          (if (and (pair? cs) (= (car (car cs)) key))
                            (begin
                              (add-pending! (pool-length) atom (cdr (car cs)))
                              (push-word! 0)
                              (loop (+ offset 1) (cdr cs)))
                            (begin
                              (add-pending! (pool-length) atom default)
                              (push-word! 0)
                              (loop (+ offset 1) cs)))))))]
                 [else
                  ;; The count shares its word with the dense flag, so it must
                  ;; stay below bit 31.
                  (when (>= (length cases) (expt 2 31))
                    (assertion-violation 'link "switch has too many cases" name))
                  (push-word! (length cases))
                  (for-each
                    (lambda (c)
                      (push-word! (bitwise-and (car c) u32-mask))
                      (add-pending! (pool-length) atom (cdr c))
                      (push-word! 0))
                    cases)])))]
          [else                              ; sym
           (let* ([atom (cadr fixup)] [mask (caddr fixup)] [default (cadddr fixup)]
                  [cases (list-ref fixup 4)]
                  [table (pool-length)])
             ;; operand 2 of switch-symbol: (op src table)
             (patch-operand! atom 2 table)
             (add-pending! (pool-length) atom default)
             (push-word! 0)
             (push-word! (length cases))
             (push-word! (bitwise-and mask u32-mask))
             (push-word! (bitwise-and (bitwise-arithmetic-shift-right mask 32) u32-mask))
             (for-each
               (lambda (c)
                 (push-word! (bitwise-and (car c) u32-mask))
                 (push-word! (bitwise-and (bitwise-arithmetic-shift-right (car c) 32) u32-mask))
                 (add-pending! (pool-length) atom (cdr c))
                 (push-word! 0))
               cases))]))
      switch-fixups)
    (when case-rows
      ;; The enter-case instruction is atom 0: (enter-case table frame).
      (patch-operand! 0 1 (pool-length))
      (push-word! (length case-rows))
      (for-each
        (lambda (row)
          (push-word! (vector-ref row 0))
          (push-word! (vector-ref row 1))
          (add-pending! (pool-length) 0 (vector-ref row 2))
          (push-word! 0)
          (push-word! (vector-ref row 3))
          (push-word! (vector-ref row 4))
          (push-word! (vector-ref row 5)))
        case-rows))
    (let* ([insns (vector->list atoms)]
           [pool-vec (list->vector (gvec->list pool))]
           [cp (make-compiled-procedure code-id insns
                 (gvec->list (arena-constants arena))
                 pool-vec arity size name meta source
                 '()                          ; spans, set below
                 '() #f #f #f)])
      (encode-procedure! cp)
      (let* ([code (compiled-procedure-code cp)]
             [atom-offsets (compiled-procedure-atom-offsets cp)]
             [code-len (bytevector-length code)]
             [atom-count (vector-length atoms)]
             [label-offsets (make-eqv-hashtable)])
        (let loop ([i 0] [l insns])
          (unless (null? l)
            (when (eq? (car (car l)) 'label)
              (hashtable-set! label-offsets (cadr (car l)) (vector-ref atom-offsets i)))
            (loop (+ i 1) (cdr l))))
        (for-each
          (lambda (p)
            (let* ([slot (car p)] [atom (cadr p)] [target (caddr p)]
                   [disp (- (hashtable-ref label-offsets target #f)
                           (vector-ref atom-offsets atom))])
              (unless (and (>= disp (- (expt 2 31))) (< disp (expt 2 31)))
                (assertion-violation 'link "switch displacement overflows i32" name))
              (vector-set! pool-vec slot (bitwise-and disp u32-mask))))
          (reverse pending))
        ;; Source runs.
        (let ([spans '()] [span-count 0]
              [span-ids (make-eq-hashtable)]
              [runs '()])                    ; newest first
          (let loop ([notes sources])
            (unless (null? notes)
              (let* ([atom (car (car notes))]
                     [source (cdr (car notes))]
                     [end-atom (if (pair? (cdr notes)) (car (cadr notes)) atom-count)])
                (when (> end-atom atom)
                  (let* ([start (vector-ref atom-offsets atom)]
                         [end (if (>= end-atom atom-count)
                                code-len
                                (vector-ref atom-offsets end-atom))])
                    (when (> end start)
                      (let-values ([(flags span)
                                    (if (or (eq? source #f) (null? source))
                                      (values src-flag-synthetic 0)
                                      (values 0
                                        (or (hashtable-ref span-ids source #f)
                                          (begin
                                            (set! spans (cons source spans))
                                            (set! span-count (+ span-count 1))
                                            (hashtable-set! span-ids source span-count)
                                            span-count))))])
                        (let ([last (and (pair? runs) (car runs))])
                          (if (and last
                                   (eqv? (src-run-span last) span)
                                   (eqv? (src-run-flags last) flags)
                                   (= (+ (src-run-start last) (src-run-len last)) start))
                            (src-run-len-set! last (- end (src-run-start last)))
                            (set! runs (cons (make-src-run start (- end start) flags span)
                                         runs)))))))))
              (loop (cdr notes))))
          (compiled-procedure-srcmap-set! cp (reverse runs))
          (compiled-procedure-spans-set! cp (reverse spans))
          cp))))

  ;; =========================================================================
  ;; Program level
  ;; =========================================================================
  (define (compile-procedure procedure)
    (let* ([arena (new-arena)]
           [e (lower-procedure procedure arena)])
      (link-parts (cfg-proc-code procedure) (emitted-atoms e) (emitted-sources e)
        (emitted-switch-fixups e) arena (emitted-name e) (emitted-meta e)
        (emitted-source e) (emitted-arity e) (emitted-size e) #f)))

  (define (shift-switch-fixup fixup base)
    (cons (car fixup) (cons (+ (cadr fixup) base) (cddr fixup))))

  ;; BY-CODE: code id -> procedure of the program.
  (define (compile-case-group by-code group)
    (let* ([arena (new-arena)]
           [parts
            (map
              (lambda (code)
                (let ([procedure
                       (or (hashtable-ref by-code code #f)
                         (assertion-violation 'compile-program
                           "case-lambda clause is not in the program" code))])
                  (lower-procedure procedure arena)))
              (case-group-clauses group))]
           [frame-size (fold-left (lambda (m p) (max m (emitted-size p))) 1 parts)]
           [first-part (and (pair? parts) (car parts))]
           [name (if first-part (emitted-name first-part) #f)]
           [meta (if first-part (emitted-meta first-part) '())]
           [source (if first-part (emitted-source first-part) #f)]
           [all-atoms (list (list 'enter-case 0 frame-size))]   ; reversed below
           [all-sources '()]
           [all-fixups '()]
           [rows '()]
           [count 1])
      (for-each
        (lambda (part)
          (let* ([arity (emitted-arity part)]
                 [part-atoms (emitted-atoms part)])
            (let-values ([(mn mx rest-base rest-cnt part-atoms)
                          (if (< arity 0)
                            ;; A variadic clause: `arity = -(required + 1)'.  The
                            ;; dispatch performs the extras move into `base' and
                            ;; stores their count in `cnt', so the clause's own
                            ;; `enter_rest' is replaced by a `nop'.  The atom
                            ;; stays (indices and source runs stay stable).
                            (let ([first (car part-atoms)])
                              (unless (eq? (car first) 'enter-rest)
                                (assertion-violation 'compile-program
                                  "variadic case-lambda clause does not start with enter_rest"))
                              (values (- (- arity) 1) case-no-rest
                                (list-ref first 3) (list-ref first 2)
                                (cons (list 'nop) (cdr part-atoms))))
                            (values arity arity case-no-rest case-no-rest part-atoms))])
              (let ([label (arena-new-label! arena)])
                (set! all-atoms (cons (list 'label label) all-atoms))
                (set! count (+ count 1))
                (let ([base count])
                  (for-each (lambda (a) (set! all-atoms (cons a all-atoms))) part-atoms)
                  (set! count (+ count (length part-atoms)))
                  (for-each
                    (lambda (s) (set! all-sources (cons (cons (+ base (car s)) (cdr s)) all-sources)))
                    (emitted-sources part))
                  (for-each
                    (lambda (f) (set! all-fixups (cons (shift-switch-fixup f base) all-fixups)))
                    (emitted-switch-fixups part)))
                (set! rows
                  (cons (vector mn mx label (emitted-size part) rest-base rest-cnt) rows))))))
        parts)
      (link-parts (case-group-code group) (reverse all-atoms) (reverse all-sources)
        (reverse all-fixups) arena name meta source -2147483648 frame-size
        (reverse rows))))

  ;;; Compiling the procedures in parallel
  ;;
  ;; A procedure is lowered and encoded on its own: its arena, labels and register
  ;; allocation belong to it, and the tables the modules share are only read. So the
  ;; procedures of a big program are compiled by threads. The biggest go first, each to
  ;; the thread with the least work so far, so that the merged top level of a static link
  ;; does not wait behind others; the results are put back in order, so the output does
  ;; not depend on the threads. CAPY_BYTECODE_THREADS=N uses N threads; the default is
  ;; the calling thread only: allocating on several threads at once makes MMTk's
  ;; RawMemoryFreeList ask for a negative size and abort (seen linking simple-mc,
  ;; 2026-11-27).

  ;; Fewer items than this per thread are not worth a thread.
  (define items-per-thread 32)

  (define (thread-limit)
    (let ([flag (getenv "CAPY_BYTECODE_THREADS")])
      (or (and flag (let ([n (string->number flag)]) (and n (exact? n) (integer? n) (> n 0) n)))
        1)))

  ;; (map F ITEMS) where the calls of F may run on other threads. COST estimates the
  ;; work of an item.
  (define (parallel-map f cost items)
    (let* ([n (length items)]
           [threads (min (thread-limit) (div n items-per-thread))])
      (if (<= threads 1)
        (map f items)
        (let ([items (list->vector items)]
              [buckets (make-vector threads '())]     ; indices, in reverse
              [loads (make-vector threads 0)]
              [results (make-vector n #f)])
          (for-each
            (lambda (entry)
              (let least ([t 1] [best 0])
                (if (< t threads)
                  (least (+ t 1) (if (< (vector-ref loads t) (vector-ref loads best)) t best))
                  (begin
                    (vector-set! buckets best (cons (car entry) (vector-ref buckets best)))
                    (vector-set! loads best (+ (vector-ref loads best) (cdr entry)))))))
            (list-sort (lambda (a b) (> (cdr a) (cdr b)))
              (let loop ([i 0] [acc '()])
                (if (= i n) acc (loop (+ i 1) (cons (cons i (cost (vector-ref items i))) acc))))))
          (let* ([work (lambda (bucket)
                         (map (lambda (i) (cons i (f (vector-ref items i)))) bucket))]
                 ;; A condition a thread raises is raised again on the calling thread as
                 ;; it was, not as the &uncaught-exception of `thread-join`.
                 [run (lambda (bucket)
                        (call/cc
                          (lambda (k)
                            (with-exception-handler
                              (lambda (condition) (k (cons 'raised condition)))
                              (lambda () (cons 'done (work bucket)))))))]
                 [spawned (let loop ([t 1] [acc '()])
                            (if (= t threads)
                              (reverse acc)
                              (loop (+ t 1)
                                (let ([bucket (vector-ref buckets t)])
                                  (cons (go (lambda () (run bucket))) acc)))))]
                 [mine (work (vector-ref buckets 0))]
                 [theirs (map thread-join spawned)])
            (for-each (lambda (r) (when (eq? (car r) 'raised) (raise (cdr r)))) theirs)
            (for-each (lambda (done) (vector-set! results (car done) (cdr done)))
              (apply append mine (map cdr theirs)))
            (vector->list results))))))

  (define (procedure-size procedure)
    (fold-left (lambda (n block) (+ n 1 (length (block-instructions block))))
      0 (cfg-proc-blocks procedure)))

  (define (compile-program program)
    (define clause-codes (make-hashtable equal-hash equal?))
    (define by-code (make-hashtable equal-hash equal?))
    (for-each
      (lambda (g)
        (for-each (lambda (c) (hashtable-set! clause-codes c #t)) (case-group-clauses g)))
      (program-case-groups program))
    (for-each (lambda (p) (hashtable-set! by-code (cfg-proc-code p) p)) (program-procedures program))
    (let* ([plain (filter (lambda (p) (not (hashtable-ref clause-codes (cfg-proc-code p) #f)))
                    (program-procedures program))]
           [procedures
            (parallel-map
              (lambda (item)
                (if (eq? (car item) 'procedure)
                  (compile-procedure (cdr item))
                  (compile-case-group by-code (cdr item))))
              (lambda (item)
                (if (eq? (car item) 'procedure)
                  (procedure-size (cdr item))
                  (fold-left (lambda (n code) (+ n (procedure-size (hashtable-ref by-code code #f))))
                    0 (case-group-clauses (cdr item)))))
              (append (map (lambda (p) (cons 'procedure p)) plain)
                (map (lambda (g) (cons 'group g)) (program-case-groups program))))]
           [entry (let find ([ps procedures] [i 0])
                    (cond
                      [(null? ps)
                       (assertion-violation 'compile-program
                         "entry procedure is part of the program")]
                      [(code-id=? (compiled-procedure-code-id (car ps)) (program-entry program)) i]
                      [else (find (cdr ps) (+ i 1))]))])
      (make-compiled-program procedures entry)))
)

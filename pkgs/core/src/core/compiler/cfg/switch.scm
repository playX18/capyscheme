;; Switch inference: chains of `if (eq? x K)` style tests become one `Switch'.

(library (core compiler cfg switch)
  (export infer-switches)
  (import (core) (capy) (core compiler cfg ir) (core compiler city-hash))

  (define symbol-hash-dispatch-min-length 4)

  ;; SwitchNode
  (define-record-type snode
    (fields kind scrutinee value target next (mutable removed) instruction-count))
  ;; SwitchPrimCall
  (define-record-type pcall (fields prim args instruction-count))
  ;; SwitchCandidate
  (define-record-type candidate
    (fields kind scrutinee cases default chain-blocks instruction-count))

  (define (take-list l n)
    (if (= n 0) '() (cons (car l) (take-list (cdr l) (- n 1)))))
  (define (drop-list l n)
    (if (= n 0) l (drop-list (cdr l) (- n 1))))

  (define (infer-switches procedure)
    (let loop ()
      (let ([predecessors (local-predecessor-counts procedure)]
            ;; Built once per iteration, not per attempt (quadratic otherwise).
            [blocks (make-eqv-hashtable)])
        (for-each (lambda (b) (hashtable-set! blocks (block-id b) b)) (cfg-proc-blocks procedure))
        (let ([cand
               (let find ([bs (cfg-proc-blocks procedure)])
                 (cond
                   [(null? bs) #f]
                   [(infer-switch-candidate blocks (cfg-proc-blocks procedure) (block-id (car bs))
                      predecessors)]
                   [else (find (cdr bs))]))])
          (when cand
            (apply-switch-candidate! procedure cand)
            (loop)))))
    procedure)

  (define (local-predecessor-counts procedure)
    (let ([counts (make-eqv-hashtable)])
      (for-each
        (lambda (b)
          (for-each
            (lambda (s) (hashtable-set! counts s (+ 1 (hashtable-ref counts s 0))))
            (term-successors (block-terminator b))))
        (cfg-proc-blocks procedure))
      counts))

  (define (pred-count predecessors b) (hashtable-ref predecessors b 0))

  (define (block-ref blocks id)
    (or (hashtable-ref blocks id #f)
      (error 'infer-switches "no such block" id)))

  (define (set-of list)
    (let ([s (make-eqv-hashtable)])
      (for-each (lambda (x) (hashtable-set! s x #t)) list)
      s))

  (define (infer-switch-candidate blocks procedure-blocks start predecessors)
    (let ([first (switch-node blocks predecessors (block-ref blocks start) #t)])
      (and first
        (let loop ([kind (snode-kind first)]
                   [scrutinee (snode-scrutinee first)]
                   [cases (list (make-switch-case (snode-value first) (snode-target first)))]
                   [chain (append (reverse (snode-removed first)) (list start))]
                   [instruction-count (snode-instruction-count first)]
                   [seen (list (snode-value first))]
                   [next (snode-next first)])
          ;; `cases' and `chain' are accumulated in reverse here.
          (let* ([step (switch-chain-next-block blocks predecessors next)]
                 [node (and step
                         (let ([b (hashtable-ref blocks (car step) #f)])
                           (and b (switch-node blocks predecessors b #f))))])
            (if (and node
                     (equal? (snode-kind node) kind)
                     (operand=? (snode-scrutinee node) scrutinee)
                     (not (exists (lambda (v) (switch-case-value=? v (snode-value node))) seen)))
              (loop kind scrutinee
                (cons (make-switch-case (snode-value node) (snode-target node)) cases)
                (append (reverse (snode-removed node))
                  (cons (car step) (append (reverse (cdr step)) chain)))
                instruction-count
                (cons (snode-value node) seen)
                (snode-next node))
              (finish-candidate blocks procedure-blocks start predecessors kind scrutinee
                (reverse cases) (reverse chain) instruction-count next)))))))

  (define (finish-candidate blocks procedure-blocks start predecessors kind scrutinee cases
            chain-blocks instruction-count next)
    (define symbol-kind? (pair? kind))   ; (SymbolEq . mask)
    (and
      (not (or (< (length cases) 2)
               (and symbol-kind? (<= (length cases) symbol-hash-dispatch-min-length))))
      (let* ([mask (and symbol-kind? (symbol-hash-dispatch-mask (length cases)))]
             [cases (if symbol-kind? (bucket-symbol-switch-cases cases mask) cases)]
             [kind (if symbol-kind? (cons 'SymbolEq mask) kind)]
             [removed (set-of (cdr chain-blocks))]
             [removed-defs (switch-removed-defs blocks start chain-blocks instruction-count)])
        (and
          (not
            (or
              (exists (lambda (c) (branch-target-mentions-removed? (switch-case-target c) removed))
                cases)
              (branch-target-mentions-removed? next removed)
              (operand-mentions-defs? scrutinee removed-defs)
              (exists (lambda (c) (branch-target-mentions-defs? (switch-case-target c) removed-defs))
                cases)
              (branch-target-mentions-defs? next removed-defs)
              (surviving-blocks-mention-defs? procedure-blocks start removed removed-defs
                instruction-count)))
          (make-candidate kind scrutinee cases next chain-blocks instruction-count)))))

  (define (switch-chain-next-block blocks predecessors next)
    ;; -> (block . skipped-blocks) or #f
    (and (target-local? next)
      (null? (target-local-edge-assigns next))
      (= (pred-count predecessors (target-local-block next)) 1)
      (let loop ([block (target-local-block next)] [skipped '()] [visited '()])
        (if (memv block visited)
          #f   ; empty jump cycle, stop rather than spinning forever
          (let ([current (block-ref blocks block)])
            (cond
              [(switch-node blocks predecessors current #f) (cons block (reverse skipped))]
              [(not (term-jump? (block-terminator current))) (cons block (reverse skipped))]
              [else
               (let ([target (term-jump-target (block-terminator current))])
                 (if (or (pair? (block-instructions current))
                         (not (= (pred-count predecessors target) 1)))
                   (cons block (reverse skipped))
                   (loop target (cons block skipped) (cons block visited))))]))))))

  (define (switch-node blocks predecessors block allow-prefix)
    (or (switch-direct-node block allow-prefix)
      (switch-jump-branch-node blocks predecessors block allow-prefix)
      (switch-branch-prim-node block allow-prefix)
      (switch-jump-branch-prim-node blocks predecessors block allow-prefix)))

  (define (switch-direct-node block allow-prefix)
    (let ([t (block-terminator block)])
      (and (term-branch? t)
        (operand-local? (term-branch-test t))
        (switch-test-node block (operand-local-uvar (term-branch-test t))
          (term-branch-consequent t) (term-branch-alternative t) allow-prefix))))

  (define (switch-jump-branch-node blocks predecessors block allow-prefix)
    (let ([t (block-terminator block)])
      (and (term-jump? t)
        (= (pred-count predecessors (term-jump-target t)) 1)
        (let* ([target (term-jump-target t)]
               [branch (block-ref blocks target)]
               [bt (block-terminator branch)])
          (and (null? (block-instructions branch))
            (term-branch? bt)
            (operand-local? (term-branch-test bt))
            ;; Mutable uvars: the test home is shared, no edge-arg remapping.
            (let ([node (switch-test-node block (operand-local-uvar (term-branch-test bt))
                          (term-branch-consequent bt) (term-branch-alternative bt)
                          allow-prefix)])
              (and node
                (begin
                  (snode-removed-set! node (append (snode-removed node) (list target)))
                  node))))))))

  (define (switch-test-node block test consequent alternative allow-prefix)
    (or
      (let ([call (switch-prim-call block test allow-prefix)])
        (and call (switch-direct-comparison-node call consequent alternative)))
      (switch-char-node block test consequent alternative allow-prefix)))

  (define (switch-branch-prim-node block allow-prefix)
    (let ([t (block-terminator block)])
      (and (term-branch-prim? t)
        (switch-branch-prim-test-node (block-instructions block) (term-branch-prim-prim t)
          (term-branch-prim-args t) (term-branch-prim-consequent t)
          (term-branch-prim-alternative t) allow-prefix))))

  (define (switch-jump-branch-prim-node blocks predecessors block allow-prefix)
    (let ([t (block-terminator block)])
      (and (term-jump? t)
        (= (pred-count predecessors (term-jump-target t)) 1)
        (let* ([target (term-jump-target t)]
               [branch (block-ref blocks target)]
               [bt (block-terminator branch)])
          (and (null? (block-instructions branch))
            (term-branch-prim? bt)
            (let ([node (switch-branch-prim-test-node (block-instructions block)
                          (term-branch-prim-prim bt) (term-branch-prim-args bt)
                          (term-branch-prim-consequent bt) (term-branch-prim-alternative bt)
                          allow-prefix)])
              (and node
                (begin
                  (snode-removed-set! node (append (snode-removed node) (list target)))
                  node))))))))

  (define (switch-branch-prim-test-node instructions prim args consequent alternative
            allow-prefix)
    (or
      (let ([call (switch-branch-prim-call instructions prim args allow-prefix)])
        (and call (switch-direct-comparison-node call consequent alternative)))
      (switch-char-branch-prim-node instructions prim args consequent alternative
        allow-prefix)))

  (define (switch-direct-comparison-node call consequent alternative)
    (or (switch-fixnum-node call consequent alternative)
      (switch-eq-char-node call consequent alternative)
      (switch-eq-symbol-node call consequent alternative)))

  (define (switch-kind-for-primitive prim)
    (case prim
      [(eq? eqv? equal?) 'Eq]
      [(fx=?) 'Fixnum]
      [(=) 'Numeric]
      [else #f]))

  (define (int32? v)
    (and (exact-integer? v) (<= -2147483648 v 2147483647)))

  ;; (scrutinee . value) or #f
  (define (const-test lhs rhs pred value-of)
    (cond
      [(and (operand-constant? rhs) (pred (operand-constant-value rhs)))
       (cons lhs (value-of (operand-constant-value rhs)))]
      [(and (operand-constant? lhs) (pred (operand-constant-value lhs)))
       (cons rhs (value-of (operand-constant-value lhs)))]
      [else #f]))

  (define (switch-fixnum-node call consequent alternative)
    (let ([kind (switch-kind-for-primitive (pcall-prim call))])
      (and kind
        (= (length (pcall-args call)) 2)
        (let ([t (const-test (car (pcall-args call)) (cadr (pcall-args call)) int32?
                   (lambda (v) v))])
          (and t
            (make-snode kind (car t) (make-case-integer (cdr t)) consequent alternative '()
              (pcall-instruction-count call)))))))

  (define (eq-like-primitive? prim) (memq prim '(eq? eqv? equal?)))

  (define (switch-eq-char-node call consequent alternative)
    (and (eq-like-primitive? (pcall-prim call))
      (= (length (pcall-args call)) 2)
      (let ([t (const-test (car (pcall-args call)) (cadr (pcall-args call)) char?
                 char->integer)])
        (and t
          (make-snode 'CharEq (car t) (make-case-integer (cdr t)) consequent alternative '()
            (pcall-instruction-count call))))))

  (define (switch-eq-symbol-node call consequent alternative)
    (and (eq-like-primitive? (pcall-prim call))
      (= (length (pcall-args call)) 2)
      (let ([t (const-test (car (pcall-args call)) (cadr (pcall-args call)) symbol?
                 (lambda (v) v))])
        (and t
          (make-snode (cons 'SymbolEq 0) (car t)
            (make-case-symbol (symbol-raw-hash (cdr t)) (cdr t))
            consequent alternative '() (pcall-instruction-count call))))))

  (define (switch-prim-call block test allow-prefix)
    (let ([instructions (block-instructions block)])
      (and (pair? instructions)
        (let* ([n (length instructions)]
               [last (list-ref instructions (- n 1))])
          (and (insn-prim-call? last)
            (eqv? (insn-prim-call-dst last) test)
            (let* ([args (insn-prim-call-args last)]
                   [needed (prim-call-arg-locals args)])
              (call-with-values
                (lambda () (trailing-const-defs (take-list instructions (- n 1)) needed))
                (lambda (constants const-count)
                  (let ([instruction-count (+ 1 const-count)])
                    (and (or (= n instruction-count) allow-prefix)
                      (make-pcall (insn-prim-call-prim last)
                        (map (lambda (a) (resolve-local-constant a constants)) args)
                        instruction-count)))))))))))

  ;; The fused-test twin of switch-prim-call: the prim and its args come from
  ;; the terminator, only trailing `Const' instructions are consumed.
  (define (switch-branch-prim-call instructions prim args allow-prefix)
    (let ([needed (prim-call-arg-locals args)])
      (call-with-values
        (lambda () (trailing-const-defs instructions needed))
        (lambda (constants instruction-count)
          (and (or (= (length instructions) instruction-count) allow-prefix)
            (make-pcall prim
              (map (lambda (a) (resolve-local-constant a constants)) args)
              instruction-count))))))

  (define (prim-call-arg-locals args)
    (set-of (let loop ([as args] [acc '()])
              (cond
                [(null? as) (reverse acc)]
                [(operand-local? (car as)) (loop (cdr as) (cons (operand-local-uvar (car as)) acc))]
                [else (loop (cdr as) acc)]))))

  ;; Trailing `Const' definitions for locals in `needed', walking back from
  ;; the end of `instructions'.  Returns (values constants count): constants
  ;; is an eqv hashtable uvar -> value.  Only trailing constants count: a
  ;; mutable uvar may be rebound by an earlier non-`Const', which stops the walk.
  (define (trailing-const-defs instructions needed)
    (let ([constants (make-eqv-hashtable)])
      (let loop ([is (reverse instructions)] [count 0])
        (if (and (pair? is)
                 (insn-const? (car is))
                 (hashtable-contains? needed (insn-const-dst (car is))))
          (begin
            (hashtable-delete! needed (insn-const-dst (car is)))
            (hashtable-set! constants (insn-const-dst (car is)) (insn-const-value (car is)))
            (loop (cdr is) (+ count 1)))
          (values constants count)))))

  (define (resolve-local-constant operand constants)
    (if (operand-local? operand)
      (let ([found (hashtable-contains? constants (operand-local-uvar operand))])
        (if found
          (make-operand-constant (hashtable-ref constants (operand-local-uvar operand) #f))
          operand))
      operand))

  (define (symbol-hash-dispatch-mask ntargets)
    (let loop ([nbits 2])
      (if (and (< nbits 64) (> ntargets (bitwise-arithmetic-shift-left 1 nbits)))
        (loop (+ nbits 1))
        (if (= nbits 64)
          (- (bitwise-arithmetic-shift-left 1 64) 1)
          (- (bitwise-arithmetic-shift-left 1 nbits) 1)))))

  (define (bucket-symbol-switch-cases cases mask)
    (map
      (lambda (c)
        (let ([v (switch-case-value c)])
          (if (case-symbol? v)
            (make-switch-case
              (make-case-symbol (bitwise-and (case-symbol-hash v) mask) (case-symbol-value v))
              (switch-case-target c))
            c)))
      cases))

  (define (switch-char-node block test consequent alternative allow-prefix)
    (let* ([instructions (block-instructions block)]
           [n (length instructions)])
      (and (>= n 3)
        (or (= n 3) allow-prefix)
        (let* ([tail (drop-list instructions (- n 3))]
               [i0 (car tail)] [i1 (cadr tail)] [i2 (caddr tail)])
          (and (insn-prim-call? i0) (insn-prim-call? i1) (insn-prim-call? i2)
            (let ([scrutinee-int (insn-prim-call-dst i0)]
                  [scrutinee-args (insn-prim-call-args i0)]
                  [case-int (insn-prim-call-dst i1)]
                  [case-args (insn-prim-call-args i1)]
                  [cmp (insn-prim-call-dst i2)]
                  [cmp-args (insn-prim-call-args i2)])
              (and (eq? (insn-prim-call-prim i0) 'char->integer)
                (eq? (insn-prim-call-prim i1) 'char->integer)
                (eq? (insn-prim-call-prim i2) '=)
                (= (length scrutinee-args) 1)
                (= (length case-args) 1)
                (eqv? cmp test)
                (= (length cmp-args) 2)
                (operand-constant? (car case-args))
                (char? (operand-constant-value (car case-args)))
                (let ([a (car cmp-args)] [b (cadr cmp-args)])
                  (and (operand-local? a) (operand-local? b)
                    (or (and (eqv? (operand-local-uvar a) scrutinee-int)
                          (eqv? (operand-local-uvar b) case-int))
                      (and (eqv? (operand-local-uvar a) case-int)
                        (eqv? (operand-local-uvar b) scrutinee-int)))))
                (make-snode 'Char (car scrutinee-args)
                  (make-case-integer (char->integer (operand-constant-value (car case-args))))
                  consequent alternative '() 3))))))))

  ;; `char=?' chains through `BranchPrim': `char=?' becomes
  ;; `(= (char->integer a) (char->integer b))', fused into the branch, so the
  ;; two conversions sit in the block and the `=' is the terminator.  Either
  ;; side may be the constant case char.
  (define (switch-char-branch-prim-node instructions prim args consequent alternative
            allow-prefix)
    (and (eq? prim '=)
      (= (length args) 2)
      (operand-local? (car args))
      (operand-local? (cadr args))
      (>= (length instructions) 2)
      (let* ([lhs (operand-local-uvar (car args))]
             [rhs (operand-local-uvar (cadr args))]
             [n (length instructions)]
             [tail (drop-list instructions (- n 2))]
             [i0 (car tail)] [i1 (cadr tail)])
        (and (insn-prim-call? i0) (insn-prim-call? i1)
          (eq? (insn-prim-call-prim i0) 'char->integer)
          (eq? (insn-prim-call-prim i1) 'char->integer)
          (= (length (insn-prim-call-args i0)) 1)
          (= (length (insn-prim-call-args i1)) 1)
          (let ([convert-lhs (insn-prim-call-dst i0)] [convert-rhs (insn-prim-call-dst i1)])
            (or (and (eqv? convert-lhs lhs) (eqv? convert-rhs rhs))
              (and (eqv? convert-lhs rhs) (eqv? convert-rhs lhs))))
          (let* ([arg-l (car (insn-prim-call-args i0))]
                 [arg-r (car (insn-prim-call-args i1))]
                 [needed (prim-call-arg-locals (list arg-l arg-r))])
            (call-with-values
              (lambda () (trailing-const-defs (take-list instructions (- n 2)) needed))
              (lambda (constants const-count)
                (let ([instruction-count (+ 2 const-count)])
                  (and (or (= n instruction-count) allow-prefix)
                    (let* ([li (resolve-local-constant arg-l constants)]
                           [ri (resolve-local-constant arg-r constants)]
                           [t (cond
                                [(and (operand-constant? ri) (char? (operand-constant-value ri)))
                                 (cons li (operand-constant-value ri))]
                                [(and (operand-constant? li) (char? (operand-constant-value li)))
                                 (cons ri (operand-constant-value li))]
                                [else #f])])
                      (and t
                        (make-snode 'Char (car t) (make-case-integer (char->integer (cdr t)))
                          consequent alternative '() instruction-count))))))))))))

  (define (branch-target-mentions-removed? target removed)
    (and (target-local? target) (hashtable-contains? removed (target-local-block target))))

  (define (switch-removed-defs blocks start chain-blocks instruction-count)
    (let ([defs (make-eqv-hashtable)])
      (for-each
        (lambda (id)
          (let ([b (hashtable-ref blocks id #f)])
            (when b
              (for-each (lambda (i) (for-each (lambda (d) (hashtable-set! defs d #t)) (insn-defs i)))
                (block-instructions b)))))
        (cdr chain-blocks))
      (let ([b (hashtable-ref blocks start #f)])
        (when b
          (let* ([is (block-instructions b)]
                 [consumed-start (max 0 (- (length is) instruction-count))])
            (for-each (lambda (i) (for-each (lambda (d) (hashtable-set! defs d #t)) (insn-defs i)))
              (drop-list is consumed-start)))))
      defs))

  (define (operand-mentions-defs? operand defs)
    (and (operand-local? operand) (hashtable-contains? defs (operand-local-uvar operand))))

  (define (branch-target-mentions-defs? target defs)
    (exists (lambda (o) (operand-mentions-defs? o defs)) (target-uses target)))

  (define (surviving-blocks-mention-defs? blocks start removed defs instruction-count)
    (define (mentions? operands)
      (exists (lambda (o) (operand-mentions-defs? o defs)) operands))
    (exists
      (lambda (block)
        (cond
          [(hashtable-contains? removed (block-id block)) #f]
          [(eqv? (block-id block) start)
           (let* ([is (block-instructions block)]
                  [keep (max 0 (- (length is) instruction-count))])
             (exists (lambda (i) (mentions? (insn-uses i))) (take-list is keep)))]
          [else
           (or (exists (lambda (i) (mentions? (insn-uses i))) (block-instructions block))
             (mentions? (term-uses (block-terminator block))))]))
      blocks))

  (define (apply-switch-candidate! procedure cand)
    (let* ([chain (candidate-chain-blocks cand)]
           [removed (set-of (cdr chain))]
           [start (car chain)])
      (let find ([bs (cfg-proc-blocks procedure)])
        (cond
          [(null? bs) #f]
          [(eqv? (block-id (car bs)) start)
           (let* ([b (car bs)]
                  [is (block-instructions b)]
                  [keep (- (length is) (candidate-instruction-count cand))])
             (when (< keep 0)
               (error 'infer-switches "switch candidate consumes more than the block instructions"))
             (block-instructions-set! b (take-list is keep))
             (block-terminator-set! b
               (make-term-switch (candidate-kind cand) (candidate-scrutinee cand)
                 (candidate-cases cand) (candidate-default cand))))]
          [else (find (cdr bs))]))
      (cfg-proc-blocks-set! procedure
        (filter (lambda (b) (not (hashtable-contains? removed (block-id b))))
          (cfg-proc-blocks procedure)))))
)

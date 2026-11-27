;; Global CFG effect analysis and dead-instruction elimination.

(library (core compiler cfg effects)
  (export
    memory-none memory-top memory-heap memory-variables memory-continuations memory-runtime
    memory-unknown memory-region
    flag-none flag-terminal flag-exits-sideways flag-control-dependent flag-reads-mutable
    flag-writes-local-state flag-reads-local-state flag-reads-pinned flag-writes-pinned
    flag-fence flag-may-call flag-may-raise flag-allocates
    sem-none sem-mutable-lexical sem-fluid sem-definite-bailout sem-possible-bailout
    sem-zero-values sem-allocation sem-type-check sem-car sem-cdr sem-vector sem-variable
    sem-string sem-bytevector sem-unknown sem-all
    bits-contains? bits-overlaps? bits-union bits-without bits-empty?
    make-effects effects? effects-flags effects-depends-on effects-causes effects-reads
    effects-writes
    effects-pure effects-for-call effects-raise effects-read effects-write effects-allocate
    effects-checked effects-checked-write effects-union effects-exclude-causes
    effects-effect-free? effects-allocation-only? effects-must-execute? effects-interferes?
    make-block-effects block-effects? block-effects-instructions block-effects-terminator
    analyze-procedure
    eliminate-dead-instructions
    instruction-effects terminator-effects cfg-primitive-effects)
  (import (core) (capy) (core compiler cfg ir))

  (define (bits-contains? a b) (= (bitwise-and a b) b))
  (define (bits-overlaps? a b) (not (= (bitwise-and a b) 0)))
  (define (bits-union a b) (bitwise-ior a b))
  (define (bits-without a b) (bitwise-and a (bitwise-not b)))
  (define (bits-empty? a) (= a 0))

  ;; MemoryRegion: Heap 0, Variables 1, Continuations 2, Runtime 3, Unknown 4.
  (define memory-none 0)
  (define (memory-region index) (bitwise-arithmetic-shift-left 1 index))
  (define memory-heap (memory-region 0))
  (define memory-variables (memory-region 1))
  (define memory-continuations (memory-region 2))
  (define memory-runtime (memory-region 3))
  (define memory-unknown (memory-region 4))
  (define memory-top (- (bitwise-arithmetic-shift-left 1 5) 1))

  (define flag-none 0)
  (define flag-terminal (bitwise-arithmetic-shift-left 1 0))
  (define flag-exits-sideways (bitwise-arithmetic-shift-left 1 1))
  (define flag-control-dependent (bitwise-arithmetic-shift-left 1 2))
  (define flag-reads-mutable (bitwise-arithmetic-shift-left 1 3))
  (define flag-writes-local-state (bitwise-arithmetic-shift-left 1 4))
  (define flag-reads-local-state (bitwise-arithmetic-shift-left 1 5))
  (define flag-reads-pinned (bitwise-arithmetic-shift-left 1 6))
  (define flag-writes-pinned (bitwise-arithmetic-shift-left 1 7))
  (define flag-fence (bitwise-arithmetic-shift-left 1 8))
  (define flag-may-call (bitwise-arithmetic-shift-left 1 9))
  (define flag-may-raise (bitwise-arithmetic-shift-left 1 10))
  (define flag-allocates (bitwise-arithmetic-shift-left 1 11))

  (define sem-none 0)
  (define sem-mutable-lexical (bitwise-arithmetic-shift-left 1 0))
  (define sem-fluid (bitwise-arithmetic-shift-left 1 2))
  (define sem-definite-bailout (bitwise-arithmetic-shift-left 1 3))
  (define sem-possible-bailout (bitwise-arithmetic-shift-left 1 4))
  (define sem-zero-values (bitwise-arithmetic-shift-left 1 5))
  (define sem-allocation (bitwise-arithmetic-shift-left 1 6))
  (define sem-type-check (bitwise-arithmetic-shift-left 1 7))
  (define sem-car (bitwise-arithmetic-shift-left 1 8))
  (define sem-cdr (bitwise-arithmetic-shift-left 1 9))
  (define sem-vector (bitwise-arithmetic-shift-left 1 10))
  (define sem-variable (bitwise-arithmetic-shift-left 1 11))
  (define sem-string (bitwise-arithmetic-shift-left 1 12))
  (define sem-bytevector (bitwise-arithmetic-shift-left 1 13))
  (define sem-unknown (bitwise-arithmetic-shift-left 1 14))
  (define sem-all (- (bitwise-arithmetic-shift-left 1 15) 1))

  (define-record-type effects (fields flags depends-on causes reads writes))

  (define (effects-pure) (make-effects flag-none sem-none sem-none memory-none memory-none))

  (define (union* . bs) (fold-left bits-union 0 bs))

  (define (effects-for-call)
    (make-effects
      (union* flag-exits-sideways flag-control-dependent flag-reads-mutable flag-reads-pinned
        flag-writes-pinned flag-may-call flag-may-raise flag-allocates)
      sem-all
      (bits-without sem-all sem-definite-bailout)
      memory-top
      memory-top))

  (define (effects-raise)
    (make-effects
      (union* flag-terminal flag-exits-sideways flag-may-raise)
      sem-none
      (bits-union sem-definite-bailout sem-possible-bailout)
      memory-continuations
      memory-none))

  (define (effects-read region semantic)
    (make-effects (bits-union flag-control-dependent flag-reads-mutable) semantic sem-none
      region memory-none))

  (define (effects-checked e)
    (make-effects (bits-union (effects-flags e) flag-may-raise) (effects-depends-on e)
      (bits-union (effects-causes e) sem-type-check) (effects-reads e) (effects-writes e)))

  (define (effects-write region semantic)
    (make-effects flag-none sem-none semantic memory-none region))

  (define (effects-checked-write e) (effects-checked e))

  (define (effects-allocate)
    (make-effects flag-allocates sem-none sem-allocation memory-none memory-heap))

  (define (effects-union a b)
    (make-effects
      (bits-union (effects-flags a) (effects-flags b))
      (bits-union (effects-depends-on a) (effects-depends-on b))
      (bits-union (effects-causes a) (effects-causes b))
      (bits-union (effects-reads a) (effects-reads b))
      (bits-union (effects-writes a) (effects-writes b))))

  ;; Remove causes that are known to be irrelevant to a consumer.
  (define (effects-exclude-causes e sems)
    (make-effects (effects-flags e) (effects-depends-on e)
      (bits-without (effects-causes e) sems) (effects-reads e) (effects-writes e)))

  (define (flags-has? e f) (bits-contains? (effects-flags e) f))

  (define blocking-flags
    (union* flag-terminal flag-exits-sideways flag-may-call flag-may-raise
      flag-writes-local-state flag-writes-pinned flag-fence))

  ;; True when evaluating the operation has no observable cause and can be
  ;; discarded when its result is unused.
  (define (effects-effect-free? e)
    (and (bits-empty? (effects-causes e))
      (not (flags-has? e blocking-flags))
      (bits-empty? (effects-writes e))))

  ;; True when the operation only allocates a fresh object.
  (define (effects-allocation-only? e)
    (and (bits-contains? (effects-causes e) sem-allocation)
      (bits-empty? (bits-without (effects-causes e) sem-allocation))
      (not (flags-has? e blocking-flags))
      (bits-empty? (effects-reads e))
      (or (bits-empty? (effects-writes e))
        (and (bits-contains? (effects-writes e) memory-heap)
          (bits-empty? (bits-without (effects-writes e) memory-heap))))))

  (define (effects-must-execute? e) (not (effects-effect-free? e)))

  (define (effects-interferes? a b)
    (define (fl e f) (flags-has? e f))
    (define (wr e) (not (bits-empty? (effects-writes e))))
    (or
      (and (fl a flag-terminal)
        (or (fl b flag-terminal) (fl b flag-control-dependent) (wr b)))
      (and (fl b flag-terminal)
        (or (fl a flag-control-dependent) (wr a)))
      (and (fl a flag-exits-sideways)
        (or (wr b) (fl b flag-control-dependent)))
      (and (fl b flag-exits-sideways)
        (or (wr a) (fl a flag-control-dependent)))
      (and (fl a flag-writes-local-state)
        (or (fl b flag-writes-local-state) (fl b flag-reads-local-state)))
      (and (fl b flag-writes-local-state)
        (or (fl a flag-writes-local-state) (fl a flag-reads-local-state)))
      (and (fl a flag-writes-pinned)
        (or (fl b flag-writes-pinned) (fl b flag-reads-pinned)))
      (and (fl b flag-writes-pinned)
        (or (fl a flag-writes-pinned) (fl a flag-reads-pinned)))
      (bits-overlaps? (effects-writes a) (bits-union (effects-writes b) (effects-reads b)))
      (bits-overlaps? (effects-writes b) (bits-union (effects-writes a) (effects-reads a)))
      (bits-overlaps? (effects-causes a) (effects-depends-on b))
      (bits-overlaps? (effects-causes b) (effects-depends-on a))
      (and (fl a flag-fence) (fl b flag-fence))))

  (define-record-type block-effects (fields instructions terminator))

  ;; One effect fact per CFG instruction and terminator.  Returns an alist
  ;; (block-id . block-effects) in block order.
  (define (analyze-procedure procedure)
    (map
      (lambda (block)
        (cons (block-id block)
          (make-block-effects
            (map instruction-effects (block-instructions block))
            (terminator-effects (block-terminator block)))))
      (cfg-proc-blocks procedure)))

  ;; Removes definitions whose values are unused and whose instructions have
  ;; no observable effects, including pure allocations of fresh objects.
  ;; Mutates and returns `procedure'.
  (define (eliminate-dead-instructions procedure)
    (let loop ()
      (let ([uses (make-eqv-hashtable)])
        (define (count! operands)
          (for-each
            (lambda (o)
              (when (operand-local? o)
                (let ([v (operand-local-uvar o)])
                  (hashtable-set! uses v (+ 1 (hashtable-ref uses v 0))))))
            operands))
        (for-each
          (lambda (block)
            (for-each (lambda (i) (count! (insn-uses i))) (block-instructions block))
            (count! (term-uses (block-terminator block))))
          (cfg-proc-blocks procedure))
        (let ([changed #f])
          (for-each
            (lambda (block)
              (block-instructions-set! block
                (filter
                  (lambda (i)
                    (let* ([e (instruction-effects i)]
                           [d (insn-def i)]
                           [removable
                             (and d
                               (= (hashtable-ref uses d 0) 0)
                               (or (effects-effect-free? e) (effects-allocation-only? e)))])
                      (when removable (set! changed #t))
                      (not removable)))
                  (block-instructions block))))
            (cfg-proc-blocks procedure))
          (if changed (loop) procedure)))))

  (define (instruction-effects i)
    (cond
      [(or (insn-assign? i) (insn-const? i)) (effects-pure)]
      [(or (insn-make-closure? i) (insn-make-case-closure? i) (insn-make-env? i)
           (insn-rest-to-list? i))
       (effects-allocate)]
      [(or (insn-env-ref? i) (insn-closure-ref? i) (insn-cache-ref? i))
       (effects-read memory-heap sem-unknown)]
      [(or (insn-env-set? i) (insn-closure-set? i) (insn-cache-set? i))
       (effects-write memory-heap sem-unknown)]
      [(insn-prim-call? i) (cfg-primitive-effects (insn-prim-call-prim i))]
      [(or (insn-rest-ref? i) (insn-rest-length? i) (insn-rest-predicate? i))
       (effects-checked (effects-read memory-heap sem-unknown))]
      [(insn-receive? i) (effects-pure)]
      [else (effects-allocate)]))   ; receive-rest

  (define (terminator-effects t)
    (cond
      [(term-raise? t) (effects-raise)]
      [(or (term-call? t) (term-tail-call? t) (term-return? t)) (effects-for-call)]
      [else (effects-pure)]))

  ;; Keyed by primitive name; first match wins in Rust, no name appears twice.
  ;; Anything not listed is `for_call'.
  (define primitive-effects-table
    (let ([table (make-eq-hashtable)])
      (define (add! names thunk)
        (for-each (lambda (n) (hashtable-set! table n thunk)) names))
      (add! '(procedure? variable? eof-object? null? pair? list? vector? bytevector? string?
              boolean? symbol? eq? eqv? equal? exact-integer? integer? char? number? complex?
              nan? real? rational? inexact? exact? fixnum? flonum? not)
        effects-pure)
      ;; Checked fixnum/flonum ops: may raise on wrong type.
      (add! '(fx+ fx- fx* fx<? fx<=? fx>? fx>=? fx=? fxand fxior fxxor fxnot
              fxarithmetic-shift-left fxarithmetic-shift-right fxzero? fxpositive?
              fxnegative? fxodd? fxeven? fxmin fxmax fl+ fl- fl* fl/ fl<? fl<=? fl>? fl>=?
              fl=? flzero? flpositive? flnegative? flnan? flinfinite? flfinite? flmin flmax
              flabs flfloor flceiling fltruncate flround flsin flcos fltan flexp fllog flasin
              flacos flsqrt flatan)
        (lambda () (effects-checked (effects-pure))))
      ;; Overflow-checked fixnum ops.
      (add! '(fx+/ovf? fx-/ovf? fx*/ovf?)
        (lambda ()
          (make-effects (bits-union flag-control-dependent flag-may-raise) sem-none
            (bits-union sem-possible-bailout sem-type-check) memory-none memory-none)))
      (add! '(car) (lambda () (effects-checked (effects-read memory-heap sem-car))))
      (add! '(cdr) (lambda () (effects-checked (effects-read memory-heap sem-cdr))))
      (add! '(set-car!) (lambda () (effects-checked-write (effects-write memory-heap sem-car))))
      (add! '(set-cdr!) (lambda () (effects-checked-write (effects-write memory-heap sem-cdr))))
      (add! '(vector-ref) (lambda () (effects-checked (effects-read memory-heap sem-vector))))
      (add! '(string-ref) (lambda () (effects-checked (effects-read memory-heap sem-string))))
      (add! '(string-set!)
        (lambda () (effects-checked-write (effects-write memory-heap sem-string))))
      (add! '(bytevector-u8-ref bytevector-s8-ref bytevector-u16-native-ref
              bytevector-s16-native-ref bytevector-u32-native-ref bytevector-s32-native-ref
              bytevector-u64-native-ref bytevector-s64-native-ref
              bytevector-ieee-single-native-ref bytevector-ieee-double-native-ref)
        (lambda () (effects-checked (effects-read memory-heap sem-bytevector))))
      (add! '(bytevector-u8-set! bytevector-s8-set! bytevector-u16-native-set!
              bytevector-s16-native-set! bytevector-u32-native-set! bytevector-s32-native-set!
              bytevector-u64-native-set! bytevector-s64-native-set!
              bytevector-ieee-single-native-set! bytevector-ieee-double-native-set!)
        (lambda () (effects-checked-write (effects-write memory-heap sem-bytevector))))
      ;; Raw foreign memory is invisible to the compiler: keep these in order
      ;; and never drop them.
      (add! '(pointer-ref/u8 pointer-set!/u8 pointer-ref/s8 pointer-set!/s8 pointer-ref/u16
              pointer-set!/u16 pointer-ref/s16 pointer-set!/s16 pointer-ref/u32
              pointer-set!/u32 pointer-ref/s32 pointer-set!/s32 pointer-ref/u64
              pointer-set!/u64 pointer-ref/s64 pointer-set!/s64 pointer-ref/f32
              pointer-set!/f32 pointer-ref/f64 pointer-set!/f64)
        effects-for-call)
      ;; Hash table and fluid primitives (mirror prims/effects.rs): leaf Rust helpers that run
      ;; no Scheme code, so they never capture a continuation.
      (add! '(%core-hash-ref/2 %core-hash-ref/3 %core-hash-contains?)
        (lambda () (effects-checked (effects-read memory-heap sem-unknown))))
      (add! '(%core-hash-plain?) (lambda () (effects-read memory-heap sem-unknown)))
      (add! '(%core-hash-put! %core-hash-try-set!)
        (lambda ()
          (make-effects
            (union* flag-control-dependent flag-reads-mutable flag-may-raise flag-allocates)
            sem-unknown
            (bits-union sem-unknown sem-type-check)
            memory-heap
            memory-heap)))
      (add! '(%fluid-ref) (lambda () (effects-checked (effects-read memory-runtime sem-fluid))))
      (add! '(%fluid-set!)
        (lambda ()
          (make-effects
            (union* flag-control-dependent flag-reads-mutable flag-may-raise flag-allocates)
            sem-fluid
            (bits-union sem-fluid sem-type-check)
            memory-runtime
            memory-runtime)))
      (add! '(vector-set!)
        (lambda () (effects-checked-write (effects-write memory-heap sem-vector))))
      (add! '(variable-ref)
        (lambda () (effects-checked (effects-read memory-variables sem-variable))))
      (add! '(variable-set!) (lambda () (effects-write memory-variables sem-variable)))
      (add! '(cache-ref) (lambda () (effects-read memory-runtime sem-unknown)))
      (add! '(cache-set!) (lambda () (effects-write memory-runtime sem-unknown)))
      ;; Slot access may run Scheme through its slow path: never dead.
      (add! '(slot-ref slot-set!) effects-for-call)
      (add! '(cons list vector tuple) effects-allocate)
      ;; The sized constructors raise on bad sizes, so they are never dead.
      (add! '(make-tuple make-vector) (lambda () (effects-checked (effects-allocate))))
      (add! '(unspecified unspecified?) effects-pure)
      (add! '(variable-bound?)
        (lambda () (effects-checked (effects-read memory-variables sem-variable))))
      (add! '(+ - * / = < > >= <= sqrt atan asin acos sin cos tan floor ceiling truncate
              quotient remainder modulo even? odd? zero?)
        (lambda ()
          (make-effects flag-may-raise sem-none sem-type-check memory-none memory-none)))
      (add! '(length)
        (lambda ()
          (effects-checked (effects-read memory-heap (bits-union sem-car sem-cdr)))))
      (add! '(string-length)
        (lambda () (effects-checked (effects-read memory-heap sem-string))))
      (add! '(bytevector-length)
        (lambda () (effects-checked (effects-read memory-heap sem-bytevector))))
      table))

  (define (cfg-primitive-effects prim)
    (let ([thunk (hashtable-ref primitive-effects-table prim #f)])
      (if thunk (thunk) (effects-for-call))))
)

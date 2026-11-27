;; The bytecode encoder: narrow / wide16 / wide32 instruction
;; encoding, label fixups and metadata counts, driven by (core compiler bytecode
;; isa).  Byte-exact port of bytecodegen's `link` as used by lower.rs `link_parts'.
;; Encoding spec: docs/BYTECODE.md "Encoding (for the Scheme assembler)".
;;
;; Counterpart of the bytecodegen `link` step that crates/capy/src/compiler/
;; bytecode/lower.rs `link_parts' calls (bc::link / LinkedBytecodeView).
;;
;; ----------------------------------------------------------------------------
;; INTERFACE THAT THE REAL encode.scm MUST IMPLEMENT
;; ----------------------------------------------------------------------------
;;
;;   (encode-procedure! cp)         cp: a compiled-procedure
;;                                  (see (core compiler bytecode types))
;;
;; INPUT  (compiled-procedure-insns cp): a list of entries
;;
;;     (label L)                 pseudo entry: binds label L (a fixnum) to the
;;                               offset of the NEXT instruction entry (or to
;;                               the code length when none follows).  Rust:
;;                               `bc::bind(label)`.  Labels are unique per
;;                               compiled procedure.
;;     (opcode operand ...)      one instruction.  `opcode' is the kebab-case
;;                               symbol of the def.rs `OpXxx' name without the
;;                               `Op' prefix (OpJumpIfNull -> jump-if-null,
;;                               OpCallPrimitive2 -> call-primitive2,
;;                               OpCallPrimitiveN -> call-primitive-n).
;;                               Operands are exactly the `args { ... }' of
;;                               bytecode_list! in crates/capy/src/bytecode/
;;                               def.rs, in declaration order, WITHOUT the
;;                               `metadata { ... }' fields (metadata slots are
;;                               allocated by the encoder / linker, they are
;;                               never operands).  Operand values:
;;                                 Register, Count, Constant, PoolReference,
;;                                 PrimitiveId, jump-less operands: non-negative
;;                                 exact integers (< 2^32)
;;                                 Immediate: exact integer in i32 range
;;                                 `#[relocatable(relative_start)] target/slow'
;;                                 operands (jump*, jump-if-*, get-slot,
;;                                 put-slot; always the LAST operand): a label
;;                                 fixnum L; the encoded operand is the signed
;;                                 displacement label-offset - instruction-start.
;;     Opcode names used by lower.scm: nop enter enter-rest return-point
;;     return-point-forward return-point-values return-point-values-rest move
;;     load-constant load-immediate load-raw cell-ref cell-set runtime-procedure
;;     closure closure-env free-ref closure-ref closure-set make-env env-ref
;;     env-set jump jump-if-true jump-if-false jump-if-null jump-if-not-null
;;     jump-if-pair jump-if-not-pair jump-if-fixnum jump-if-not-fixnum
;;     jump-if-undefined jump-if-not-undefined jump-if-eq jump-if-not-eq
;;     jump-if-less jump-if-less-equal jump-if-number-equal
;;     jump-if-greater-equal jump-if-greater jump-if-not-less
;;     jump-if-not-less-equal jump-if-not-number-equal
;;     jump-if-not-greater-equal jump-if-not-greater switch-fixnum switch-char
;;     switch-symbol loop-hint call tail-call return return-values raise
;;     rest-ref rest-len rest-null rest-pair rest-list values-rest
;;     call-primitive0 call-primitive1 call-primitive2 call-primitive3
;;     call-primitive-n car cdr cons set-car set-cdr eq not add sub mul
;;     add-immediate less less-equal number-equal greater-equal greater div
;;     quotient remainder modulo negate abs sqrt vector-ref vector-set
;;     vector-length variable-ref variable-set get-slot put-slot move-two
;;     add-immediate-move vector-ref-mul call-self tail-call-self enter-case
;;
;; WORK  Pick narrow / wide16 / wide32 per instruction exactly as bytecodegen
;;       does (the whole instruction widens; jump displacements relax to a
;;       fixpoint), emit the prefix + opcode + operands, and allocate metadata
;;       slots for opcodes that declare `metadata { ... }'.
;;
;; OUTPUT  set on cp:
;;     (compiled-procedure-code-set! cp bv)
;;         bv: bytevector of the whole encoded code block.
;;     (compiled-procedure-atom-offsets-set! cp v)
;;         v: vector of (length insns) + 1 non-negative integers.  For an
;;         instruction entry i: the byte offset of that instruction.  For a
;;         `(label L)' entry i: the byte offset of the next instruction entry
;;         (code length when there is none).  v[length insns] = code length.
;;         Rust: `atom_offsets' in `link_parts'.  lower.scm derives label
;;         offsets (switch / case-table displacements) and the srcmap byte
;;         ranges from it.
;;     (compiled-procedure-metadata-counts-set! cp m)
;;         m: vector of u32, length NUM_METADATA_KINDS, entry [kind] = the link
;;         count of the opcode of that metadata kind (`for_each_metadata!'
;;         order; Rust `metadata_kind_counts(linked.metadata_counts())').
;;
;;     The pool words (`compiled-procedure-pool') are NOT touched by the
;;     encoder: switch / case-table displacement words are patched by
;;     lower.scm after this call returns.  Pool references inside instructions
;;     are already final when encode-procedure! runs.

(library (core compiler bytecode encode)
  (export encode-procedure!)
  (import (core) (capy)
    (only (core compiler bytecode isa)
      isa-opcode-ref isa-opcodes isa-num-metadata-kinds isa-wide16-prefix isa-wide32-prefix
      isa-operand-signed?)
    (core compiler bytecode types))

  ;; Per opcode, #(number signed-flags nargs label-index metadata-kind); label-index
  ;; is the position of the `relative-start' operand or #f, metadata-kind the
  ;; metadata kind index or #f (then there is no metadata id operand).
  ;; The table is only read by key.
  (define infos (make-eq-hashtable))

  (define (opcode-info name)
    (or (hashtable-ref infos name #f)
      (let ([entry (isa-opcode-ref name)])
        (unless entry
          (assertion-violation 'encode-procedure! "unknown opcode" name))
        (let* ([operands (caddr entry)]
               [metadata (cadddr entry)]
               [info (vector (cadr entry)
                      (list->vector (map (lambda (o) (isa-operand-signed? (cadr o))) operands))
                      (length operands)
                      (let loop ([os operands] [i 0])
                        (cond [(null? os) #f]
                              [(and (pair? (cddr (car os)))
                                    (eq? (caddr (car os)) 'relative-start))
                               i]
                              [else (loop (cdr os) (+ i 1))]))
                      (and metadata (car metadata)))])
          (hashtable-set! infos name info)
          info))))

  ;; Filled once, so that threads compiling procedures (see `compile-program`) only read it.
  (define infos-filled
    (begin
      (for-each (lambda (entry) (opcode-info (car entry))) isa-opcodes)
      #t))

  (define (bits w) (* 8 w))

  ;; Does the value V fit an operand of byte width W (signed or unsigned)?
  (define (fits? v w signed?)
    (if signed?
      (let ([lim (expt 2 (- (bits w) 1))]) (and (>= v (- lim)) (< v lim)))
      (and (>= v 0) (< v (expt 2 (bits w))))))

  (define (header-len w) (case w [(1) 1] [(2) 3] [else 5]))

  ;; The smallest width in {1,2,4} for which the opcode and every operand (and
  ;; the metadata id) fit.  ARGS: the operand list, with the label operand
  ;; already replaced by its displacement (0 for the seed).
  (define (fit-width info args mid)
    (let ([opnum (vector-ref info 0)] [signed (vector-ref info 1)])
      (define (ok? w)
        (and (or (> w 1) (and (>= opnum 2) (<= opnum 255)))
             (let loop ([as args] [i 0])
               (cond
                 [(null? as) #t]
                 [(fits? (car as) w (vector-ref signed i)) (loop (cdr as) (+ i 1))]
                 [else #f]))
             (or (not mid) (fits? mid w #f))))
      (cond [(ok? 1) 1] [(ok? 2) 2] [(ok? 4) 4]
            [else (assertion-violation 'encode-procedure! "operand does not fit 32 bits" args)])))

  (define (put! bv pos w v)
    (let ([u (bitwise-and v (- (expt 2 (bits w)) 1))])
      (case w
        [(1) (bytevector-u8-set! bv pos u)]
        [(2) (bytevector-u16-set! bv pos u 'little)]
        [else (bytevector-u32-set! bv pos u 'little)])))

  (define (encode-procedure! cp)
    (let* ([insns (list->vector (compiled-procedure-insns cp))]
           [n (vector-length insns)]
           [entry-infos (make-vector n #f)]
           [widths (make-vector n 0)]
           [mids (make-vector n #f)]
           [counts (make-vector isa-num-metadata-kinds 0)]
           [per-opcode (make-eq-hashtable)]     ; opcode -> instructions so far
           [offsets (make-vector (+ n 1) 0)]
           [labels (make-eqv-hashtable)])        ; label -> offset (lookup only)
      ;; Operands of instruction I with the label operand replaced by DISP.
      (define (operands-of i disp)
        (let* ([entry (vector-ref insns i)] [info (vector-ref entry-infos i)]
               [li (vector-ref info 3)])
          (if li
            (let loop ([as (cdr entry)] [k 0])
              (cond [(null? as) '()]
                    [(= k li) (cons disp (loop (cdr as) (+ k 1)))]
                    [else (cons (car as) (loop (cdr as) (+ k 1)))]))
            (cdr entry))))
      ;; Pass 1: infos, metadata ids, counts, seed widths.
      (define (seed!)
       (let loop ([i 0])
        (when (< i n)
          (let ([entry (vector-ref insns i)])
            (unless (eq? (car entry) 'label)
              (let* ([info (opcode-info (car entry))]
                     [kind (vector-ref info 4)]
                     [mid (and kind (hashtable-ref per-opcode (car entry) 0))])
                (unless (= (length (cdr entry)) (vector-ref info 2))
                  (assertion-violation 'encode-procedure! "wrong operand count" entry))
                (vector-set! entry-infos i info)
                (when kind
                  (hashtable-set! per-opcode (car entry) (+ mid 1))
                  (vector-set! counts kind (+ (vector-ref counts kind) 1))
                  (vector-set! mids i mid))
                (vector-set! widths i (fit-width info (operands-of i 0) mid)))))
          (loop (+ i 1)))))
      (define (layout!)
        (let loop ([i 0] [off 0])
          (vector-set! offsets i off)
          (if (= i n)
            off
            (let ([entry (vector-ref insns i)])
              (cond
                [(eq? (car entry) 'label)
                 (hashtable-set! labels (cadr entry) off)
                 (loop (+ i 1) off)]
                [else
                 (let* ([info (vector-ref entry-infos i)] [w (vector-ref widths i)]
                        [nops (+ (vector-ref info 2) (if (vector-ref info 4) 1 0))])
                   (loop (+ i 1) (+ off (header-len w) (* w nops))))])))))
      (define (label-offset l)
        (or (hashtable-ref labels l #f)
          (assertion-violation 'encode-procedure! "unbound label" l)))
      (define (displacement i)
        (let ([info (vector-ref entry-infos i)])
          (- (label-offset (list-ref (cdr (vector-ref insns i)) (vector-ref info 3)))
             (vector-ref offsets i))))
      (seed!)
      ;; Relaxation: widths only grow, the whole walk uses the layout of its pass.
      (let relax ()
        (layout!)
        (let ([changed #f])
          (let loop ([i 0])
            (when (< i n)
              (let ([info (vector-ref entry-infos i)])
                (when (and info (vector-ref info 3))
                  (let ([w (fit-width info (operands-of i (displacement i)) (vector-ref mids i))])
                    (when (> w (vector-ref widths i))
                      (vector-set! widths i w)
                      (set! changed #t)))))
              (loop (+ i 1))))
          (when changed (relax))))
      ;; Emit.
      (let* ([size (vector-ref offsets n)]
             [bv (make-bytevector size 0)])
        (let loop ([i 0])
          (when (< i n)
            (let ([info (vector-ref entry-infos i)])
              (when info
                (let* ([w (vector-ref widths i)] [pos (vector-ref offsets i)]
                       [opnum (vector-ref info 0)]
                       [pos (case w
                              [(1) (bytevector-u8-set! bv pos opnum) (+ pos 1)]
                              [(2) (bytevector-u8-set! bv pos isa-wide16-prefix)
                                   (bytevector-u16-set! bv (+ pos 1) opnum 'little) (+ pos 3)]
                              [else (bytevector-u8-set! bv pos isa-wide32-prefix)
                                    (bytevector-u32-set! bv (+ pos 1) opnum 'little) (+ pos 5)])]
                       [args (operands-of i (if (vector-ref info 3) (displacement i) 0))]
                       [end (let emit ([as args] [p pos])
                              (if (null? as)
                                p
                                (begin (put! bv p w (car as)) (emit (cdr as) (+ p w)))))])
                  (when (vector-ref info 4)
                    (put! bv end w (vector-ref mids i))))))
            (loop (+ i 1))))
        (compiled-procedure-code-set! cp bv)
        (compiled-procedure-atom-offsets-set! cp offsets)
        (compiled-procedure-metadata-counts-set! cp counts))))
)

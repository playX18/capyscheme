;; Records shared by the Scheme bytecode lowering and the encoder.
;;
;; compiled-program     procedures entry
;;     procedures: list of compiled-procedure, in the order lower.rs produces
;;     them (non-clause procedures in program order, then one per case group);
;;     entry: index of the entry procedure in that list.
;;
;; compiled-procedure   code-id insns constants pool arity frame-size name meta
;;                      source spans
;;                      [mutable] srcmap code atom-offsets metadata-counts
;;     code-id          (gf . n) / (cg . n) as in the CFG IR
;;     insns            symbolic instruction list (see (core compiler bytecode
;;                      lower) header); input of `encode-procedure!'
;;     constants        list of constants of the code block, pool index order:
;;                        (value . v)   a Scheme value (Rust Constant::Value)
;;                        (code . id)   the code block of another procedure
;;                        (cell . key)  the shared cache cell with this key
;;     pool             vector of u32 words (aux pool: liveness masks and switch
;;                      / case tables; displacement words are patched after
;;                      encoding)
;;     arity            raw CodeArity: n, -(required+1) for variadic,
;;                      most-negative-i32 (-2147483648) for case-lambda
;;     frame-size       u32
;;     name meta source Scheme values copied from the CFG procedure
;;     spans            list of deduplicated span values referenced by srcmap
;;     srcmap           list of `src-run' in increasing offset order
;;     code             bytevector            [set by encode-procedure!]
;;     atom-offsets     vector of u32         [set by encode-procedure!]
;;     metadata-counts  vector of u32         [set by encode-procedure!]
;;
;; src-run              start len flags span
;;     byte range [start, start+len) of the code; flags: 1 = synthetic; span:
;;     1-based index into `spans' (0 = none).

(library (core compiler bytecode types)
  (export
    make-compiled-program compiled-program? compiled-program-procedures
    compiled-program-entry
    make-compiled-procedure compiled-procedure?
    compiled-procedure-code-id compiled-procedure-insns compiled-procedure-constants
    compiled-procedure-pool compiled-procedure-arity compiled-procedure-frame-size
    compiled-procedure-name compiled-procedure-meta compiled-procedure-source
    compiled-procedure-spans compiled-procedure-spans-set! compiled-procedure-srcmap compiled-procedure-srcmap-set!
    compiled-procedure-code compiled-procedure-code-set!
    compiled-procedure-atom-offsets compiled-procedure-atom-offsets-set!
    compiled-procedure-metadata-counts compiled-procedure-metadata-counts-set!
    make-src-run src-run? src-run-start src-run-len src-run-flags src-run-span
    src-run-len-set!
    src-flag-synthetic src-flag-hidden)
  (import (core) (capy))

  (define src-flag-synthetic 1)
  (define src-flag-hidden 2)

  (define-record-type compiled-program (fields procedures entry))

  (define-record-type compiled-procedure
    (fields code-id insns constants pool arity frame-size name meta source
      (mutable spans) (mutable srcmap) (mutable code) (mutable atom-offsets) (mutable metadata-counts)))

  (define-record-type src-run
    (fields start (mutable len) flags span))
)

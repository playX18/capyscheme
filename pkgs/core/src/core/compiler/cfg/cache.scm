;; Lower primcalls for cache access into direct cache instructions.
(library (core compiler cfg cache)
  (export lower-cache-operations)
  (import (core) (capy) (core compiler cfg ir))

  (define (lower-instruction i)
    (cond
      [(and (insn-prim-call? i) (eq? (insn-prim-call-prim i) 'cache-ref))
       (let ([args (insn-prim-call-args i)])
         (unless (and (pair? args) (null? (cdr args)))
           (error 'lower-cache-operations "cache-ref expects 1 argument" (length args)))
         (make-insn-cache-ref (insn-prim-call-dst i) (car args) (insn-prim-call-source i)))]
      [(and (insn-prim-call? i) (eq? (insn-prim-call-prim i) 'cache-set!))
       (let ([args (insn-prim-call-args i)])
         (unless (and (pair? args) (pair? (cdr args)) (null? (cddr args)))
           (error 'lower-cache-operations "cache-set! expects 2 arguments" (length args)))
         (make-insn-cache-set (insn-prim-call-dst i) (car args) (cadr args)
           (insn-prim-call-source i)))]
      [else i]))

  (define (lower-cache-operations procedure)
    (for-each
      (lambda (block)
        (block-instructions-set! block (map lower-instruction (block-instructions block))))
      (cfg-proc-blocks procedure))
    procedure))

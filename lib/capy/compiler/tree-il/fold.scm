(library (capy compiler tree-il fold)
  (export
    pre-order
    post-order
    pre-post-order)
  (import (capy compiler tree-il terms)
    (only (capy) printf)
    (rnrs))

  (define (pre-post-order pre post term)
    (define (elts-eq? a b)
      (or (null? a)
        (and (eq? (car a) (car b))
          (elts-eq? (cdr a) (cdr b)))))
    (let loop ([x term])
      (post
        (let ([x (pre x)])
          (cond
            [(or (void? x)
                (constant? x)
                (lref? x)
                (primref? x)
                (module-ref? x)
                (toplevel-ref? x))
              x]
            [(lset? x)
              (define exp* (loop (lset-value x)))
              (if (eq? exp* (lset-value x))
                x
                (make-lset (term-src x) (lset-name x) (lset-sym x) exp*))]
            [(module-set? x)
              (define exp* (loop (module-set-value x)))
              (if (eq? exp* (module-set-value x))
                x
                (make-module-set (term-src x) (module-set-module x)
                  (module-set-name x)
                  (module-set-public? x)
                  exp*))]

            [(toplevel-set? x)
              (define exp* (loop (toplevel-set-value x)))

              (if (eq? exp* (toplevel-set-value x))
                x
                (make-toplevel-set (term-src x)
                  (toplevel-set-mod x)
                  (toplevel-set-name x)
                  exp*))]
            [(toplevel-define? x)
              (define exp* (loop (toplevel-define-value x)))
              (if (eq? exp* (toplevel-define-value x))
                x
                (make-toplevel-define (term-src x)
                  (toplevel-define-mod x)
                  (toplevel-define-name x)
                  exp*))]
            [(if? x)
              (define test* (loop (if-test x)))
              (define then* (loop (if-then x)))
              (define else* (loop (if-else x)))
              (if (and (eq? test* (if-test x))
                   (eq? then* (if-then x))
                   (eq? else* (if-else x)))
                x
                (make-if (term-src x) test* then* else*))]
            [(let? x)
              (unless (list? (let-rhs x))
                (assertion-violation 'fold "malformed let" (let-rhs x) (let-ids x) (let-lhs x) (term-src x)))
              (define rhs* (map loop (let-rhs x)))
              (define body* (loop (let-body x)))
              (if (and (elts-eq? rhs* (let-rhs x))
                   (eq? body* (let-body x)))
                x
                (make-let (term-src x)
                  (let-style x)
                  (let-ids x)
                  (let-lhs x)
                  rhs*
                  body*))]
            [(fix? x)
              (unless (list? (fix-rhs x))
                (assertion-violation 'fold "malformed fix" (fix-rhs x) (term-src x)))
              (define rhs* (map loop (fix-rhs x)))
              (define body* (loop (fix-body x)))
              (if (and (elts-eq? rhs* (fix-rhs x))
                   (eq? body* (fix-body x)))
                x
                (make-fix (term-src x)
                  (fix-ids x)
                  (fix-lhs x)
                  rhs*
                  body*))]
            [(receive? x)
              (define producer* (loop (receive-producer x)))
              (define consumer* (loop (receive-consumer x)))
              (if (and (eq? producer* (receive-producer x))
                   (eq? consumer* (receive-consumer x)))
                x
                (make-receive (term-src x)
                  (receive-ids x)
                  (receive-vars x)
                  producer*
                  consumer*))]
            [(application? x)
              (define operator* (loop (application-operator x)))
              (unless (list? (application-operands x))
                (assertion-violation 'fold "malformed application" (application-operands x) (term-src x)))
              (define operands* (map loop (application-operands x)))
              (if (and (eq? operator* (application-operator x))
                   (elts-eq? operands* (application-operands x)))
                x
                (make-application (term-src x)
                  operator*
                  operands*))]
            [(primcall? x)
              (unless (list? (primcall-args x))
                (assertion-violation 'fold "malformed primcall" (primcall-prim x) (primcall-args x) (term-src x)))
              (define args* (map loop (primcall-args x)))
              (if (elts-eq? args* (primcall-args x))
                x
                (make-primcall (term-src x)
                  (primcall-prim x)
                  args*))]
            [(proc? x)
              (define body* (loop (proc-body x)))
              (if (eq? body* (proc-body x))
                x
                (make-proc (term-src x)
                  (proc-args x)
                  body*
                  (proc-meta x)
                  (proc-ids x)))]

            [(values? x)
              (unless (list? (values-values x))
                (assertion-violation 'fold "malformed values" (values-values x) (term-src x)))
              (define vals* (map loop (values-values x)))
              (if (elts-eq? vals* (values-values x))
                x
                (make-values (term-src x)
                  vals*))]
            [(sequence? x)
              (define head* (loop (sequence-head x)))
              (define tail* (loop (sequence-tail x)))
              (if (and (eq? head* (sequence-head x))
                   (eq? tail* (sequence-tail x)))
                x
                (make-sequence (term-src x)
                  head*
                  tail*))]
            [(wcm? x)
              (define mark* (loop (wcm-mark x)))
              (define result* (loop (wcm-result x)))
              (if (and (eq? mark* (wcm-mark x))
                   (eq? result* (wcm-result x)))
                x
                (make-wcm (term-src x)
                  (wcm-key x)
                  mark*
                  result*))]
            [else (error 'pre-post-order "unknown TreeIL term" x)])))))

  (define (post-order f x)
    (pre-post-order (lambda (x) x) f x))
  (define (pre-order f x)
    (pre-post-order f (lambda (x) x) x)))

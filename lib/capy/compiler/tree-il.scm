(library (capy compiler tree-il)
    (export 
        &term term? term-src 
        &lref lref-name lref-sym make-lref 
        &lset lset-name lset-sym make-lset
        &module-ref module-ref-module module-ref-name module-ref-public? make-module-ref
        &module-set module-set-module module-set-name module-set-value module-set-public? make-module-set
        &toplevel-ref toplevel-ref-mod toplevel-ref-name make-toplevel-ref
        &toplevel-set toplevel-set-mod toplevel-set-name toplevel-set-value make-toplevel
        &if if-test if-then if-else make-if 
        &let let-style let-ids let-lhs let-rhs let-body make-let let?
        &fix fix-ids fix-lhs fix-rhs fix-body make-fix fix? 
        &receive receive-ids receive-vars receive-producer receive-consumer make-receive 
        &application
        application-operator application-operands make-application
        application?

        &primcall 
        primcall? primcall-prim primcall-args make-primcall

        &primref 
        primref? primref-prim make-primref

        &constant
        constant? 
        constant-value 
        make-constant
        
        &void 
        void?
        make-void

        &proc 
        proc-args 
        proc-body
        proc-meta
        proc-ids
        make-proc
        proc?

        &values
        values?
        make-values
        values-values

        &sequence
        sequence-head
        sequence-tail
        make-sequence 

        &wcm 
        wcm-key
        wcm-mark
        wcm-result
        make-wcm
        wcm?
        
        tree-il->scheme
        pre-order 
        post-order
        pre-post-order)
    (import (capy))
    
  (define (tree-il->scheme t . ops?)
    (define ops (if (null? ops?) '() (car ops?)))
    (define strip-numeric-suffixes? (memq 'strip-numeric-suffixes? ops))
    

    (unless (term? t)
      (error 'tree-il->scheme "not a term" t))
    (cond 
      [(constant? t)
        `(quote ,(constant-value t))]
      [(void? t)
        `(unspecified)]
      [(lref? t)
        (lref-sym t)]
      [(lset? t)
        `(set! ,(lset-name t) ,(tree-il->scheme (lset-value t)))]
      [(module-ref? t)
        (define m (if (module-ref-public? t)
                      '@
                      '@@))
        `(,m ,(module-ref-module t) ,(module-ref-name t))]
      [(module-set? t)
        (define m (if (module-set-public? t)
                      '@
                      '@@))
        `(set! (,m ,(module-set-module t) ,(module-set-name t))
               ,(tree-il->scheme (module-set-value t)))]
      [(toplevel-ref? t)
        (toplevel-ref-name t)]
      [(toplevel-set? t)
        `(set! ,(toplevel-set-name t) ,(tree-il->scheme (toplevel-set-value t)))]
      [(toplevel-define? t)
        `(define ,(toplevel-define-name t) 
           ,(tree-il->scheme (toplevel-define-value t)))]
      [(if? t)
        `(if ,(tree-il->scheme (if-test t))
             ,(tree-il->scheme (if-then t))
             ,(tree-il->scheme (if-else t)))]
      [(let? t)
        (define style (let-style t))
        (define ids (let-lhs t))
        (define rhs (map tree-il->scheme (let-rhs t)))
        (define body (tree-il->scheme (let-body t)))
        `(,style
          ,(map (lambda (id rhs) `(,id ,rhs) ) ids rhs)
          ,body)]
      [(receive? t)
        (define ids (receive-ids t))
        (define vars (map tree-il->scheme (receive-vars t)))
        (define producer (tree-il->scheme (receive-producer t)))
        (define consumer (tree-il->scheme (receive-consumer t)))
        `(receive 
                  ,vars
                  ,producer
                  ,consumer)]
      [(fix? t)
        (define ids (fix-ids t))
        (define rhs (map tree-il->scheme (fix-rhs t)))
        (define body (tree-il->scheme (fix-body t)))
        `(fix ,ids
              ,rhs
              ,body)]
      [(application? t)
        (define operator (tree-il->scheme (application-operator t)))
        (define operands (map tree-il->scheme (application-operands t)))
        `(,operator ,@operands)]
      [(primcall? t)
        (define prim (primcall-prim t))
        (define args (map tree-il->scheme (primcall-args t)))
        `(,prim ,@args)]
      [(primref? t)
        (define prim (primref-prim t))
        prim]
      [(proc? t)
        (define args (proc-args t))
        (define body (tree-il->scheme (proc-body t)))
        `(lambda ,args
           ,body)]
      [(values? t)
        (define vals (map tree-il->scheme (values-values t)))
        `(values ,@vals)]
      [(sequence? t)
        (define head (tree-il->scheme (sequence-head t)))
        (define tail (tree-il->scheme (sequence-tail t)))
        `(begin ,head ,tail)]
      [(wcm? t)
        (define key (wcm-key t))
        (define mark (tree-il->scheme (wcm-mark t)))
        (define result (tree-il->scheme (wcm-result t)))
        `(with-continuation-mark ,key ,mark ,result)]))

  (define (pre-post-order pre post x)
    (define (elts-eq? a b)
      (or (null? a)
          (and (eq? (car a) (car b))
               (elts-eq? (cdr a) (cdr b)))))
  
    (let loop ([x x])
      
      (post (let ([x (pre x)])
        (cond 
          [(or (void? x)
              (constant? x)
              (lref? x)
              (primref? x)
              (module-ref? x)
              (toplevel-ref? x)
            x)]
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
            (define operands* (map loop (application-operands x)))
            (if (and (eq? operator* (application-operator x))
                    (elts-eq? operands* (application-operands x)))
              x 
              (make-application (term-src x)
                                operator*
                                operands*))]
          [(primcall? x)
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
                        result*))])))))

  (define (post-order f x)
    (pre-post-order (lambda (x) x) f x))
  (define (pre-order f x)
    (pre-post-order f (lambda (x) x) x))
)
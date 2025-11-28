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
        
        tree-il->scheme)
    (import (capy))
    
  (define (tree-il->scheme t)
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
        `(set! ,(toplevel-ref-name t) ,(tree-il->scheme (toplevel-set-value t)))]
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
)
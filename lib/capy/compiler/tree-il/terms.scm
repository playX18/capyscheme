(library (capy compiler tree-il terms)
  (export 
    &term term? term-src 
        &lref lref? lref-name lref-sym make-lref 
        &lset lset? lset-name lset-sym lset-value make-lset
        &module-ref module-ref? module-ref-module module-ref-name module-ref-public? make-module-ref
        &module-set module-set? module-set-module module-set-name module-set-value module-set-public? make-module-set
        &toplevel-ref toplevel-ref? toplevel-ref-mod toplevel-ref-name make-toplevel-ref
        &toplevel-set toplevel-set? toplevel-set-mod toplevel-set-name toplevel-set-value make-toplevel
        &toplevel-define toplevel-define? toplevel-define-mod toplevel-define-name toplevel-define-value make-toplevel-define
        &if if? if-test if-then if-else make-if 
        &let let-style let-ids let-lhs let-rhs let-body make-let let?
        &fix fix-ids fix-lhs fix-rhs fix-body make-fix fix? 
        &receive receive? receive-ids receive-vars receive-producer receive-consumer make-receive 
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
        sequence?
        sequence-head
        sequence-tail
        make-sequence 

        &wcm 
        wcm-key
        wcm-mark
        wcm-result
        make-wcm
        wcm?)
  (import (capy)))
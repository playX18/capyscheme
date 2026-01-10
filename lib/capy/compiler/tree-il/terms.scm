(library (capy compiler tree-il terms)
  (export
    &term
    term?
    term-src
    &lref
    lref?
    lref-name
    lref-sym
    make-lref
    &lset
    lset?
    lset-name
    lset-sym
    lset-value
    make-lset
    &module-ref
    module-ref?
    module-ref-module
    module-ref-name
    module-ref-public?
    make-module-ref
    &module-set
    module-set?
    module-set-module
    module-set-name
    module-set-value
    module-set-public?
    make-module-set
    &toplevel-ref
    toplevel-ref?
    toplevel-ref-mod
    toplevel-ref-name
    make-toplevel-ref
    &toplevel-set
    toplevel-set?
    toplevel-set-mod
    toplevel-set-name
    toplevel-set-value
    make-toplevel-set
    &toplevel-define
    toplevel-define?
    toplevel-define-mod
    toplevel-define-name
    toplevel-define-value
    make-toplevel-define
    &if
    if?
    if-test
    if-then
    if-else
    make-if
    &let
    let-style
    let-ids
    let-lhs
    let-rhs
    let-body
    make-let
    let?
    &fix
    fix-ids
    fix-lhs
    fix-rhs
    fix-body
    make-fix
    fix?
    &receive
    receive?
    receive-ids
    receive-vars
    receive-producer
    receive-consumer
    make-receive
    &application
    application-operator
    application-operands
    make-application
    application?

    &primcall
    primcall?
    primcall-prim
    primcall-args
    make-primcall

    &primref
    primref?
    primref-prim
    make-primref

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
    wcm?

    ~term
    ~lref
    ~lset
    ~module-ref
    ~module-set
    ~toplevel-ref
    ~toplevel-set
    ~toplevel-define
    ~if
    ~let
    ~fix
    ~receive
    ~application
    ~primcall
    ~proc
    ~void
    ~constant
    ~values
    ~sequence
    ~wcm)
  (import (capy) (srfi 257))

  ; pattern matching on TreeIL nodes
  (define-record-match-pattern
    (~term src)
    term?
    (src term-src))

  (define-record-match-pattern
    (~lref src name sym)
    lref?
    (src term-src)
    (name lref-name)
    (sym lref-sym))

  (define-record-match-pattern
    (~lset src name sym val)
    lset?
    (src term-src)
    (name lset-name)
    (sym lset-sym)
    (val lset-value))

  (define-record-match-pattern
    (~module-ref src module name public?)
    module-ref?
    (src term-src)
    (module module-ref-module)
    (name module-ref-name)
    (public? module-ref-public?))

  (define-record-match-pattern
    (~module-set src module name public? value)
    module-set?
    (src term-src)
    (module module-set-module)
    (name module-set-name)
    (public? module-set-public?)
    (value module-set-value))

  (define-record-match-pattern
    (~toplevel-ref src mod name)
    toplevel-ref?
    (src term-src)
    (mod toplevel-ref-mod)
    (name toplevel-ref-name))

  (define-record-match-pattern
    (~toplevel-set src mod name value)
    toplevel-set?
    (src term-src)
    (mod toplevel-set-mod)
    (name toplevel-set-name)
    (value toplevel-set-value))

  (define-record-match-pattern
    (~toplevel-define src mod name value)
    toplevel-define?
    (src term-src)
    (mod toplevel-define-mod)
    (name toplevel-define-name)
    (value toplevel-define-value))

  (define-record-match-pattern
    (~if src test then else)
    if?
    (src term-src)
    (test if-test)
    (then if-then)
    (else if-else))

  (define-record-match-pattern
    (~let src style ids lhs rhs body)
    let?
    (src term-src)
    (style let-style)
    (ids let-ids)
    (lhs let-lhs)
    (rhs let-rhs)
    (body let-body))

  (define-record-match-pattern
    (~fix src ids lhs rhs body)
    fix?
    (src term-src)
    (ids fix-ids)
    (lhs fix-lhs)
    (rhs fix-rhs)
    (body fix-body))

  (define-record-match-pattern
    (~receive src ids vars producer consumer)
    receive?
    (src term-src)
    (ids receive-ids)
    (vars receive-vars)
    (producer receive-producer)
    (consumer receive-consumer))

  (define-record-match-pattern
    (~application src operator operands)
    application?
    (src term-src)
    (operator application-operator)
    (operands application-operands))

  (define-record-match-pattern
    (~primcall src prim args)
    primcall?
    (src term-src)
    (prim primcall-prim)
    (args primcall-args))

  (define-record-match-pattern
    (~primref src name)
    (src term-src)
    (name primref-prim))

  (define-record-match-pattern
    (~constant src value)
    (src term-src)
    (value constant-value))

  (define-record-match-pattern
    (~void src)
    void?
    (src term-src))

  (define-record-match-pattern
    (~proc src args body meta ids)
    proc?
    (src term-src)
    (args proc-args)
    (body proc-body)
    (meta proc-meta)
    (ids proc-ids))

  (define-record-match-pattern
    (~values src vals)
    values?
    (src term-src)
    (vals values-values))

  (define-record-match-pattern
    (~sequence src head tail)
    sequence?
    (src term-src)
    (head sequence-head)
    (tail sequence-tail))

  (define-record-match-pattern
    (~wcm src key mark result)
    wcm?
    (src term-src)
    (key wcm-key)
    (mark wcm-mark)
    (result wcm-result)))

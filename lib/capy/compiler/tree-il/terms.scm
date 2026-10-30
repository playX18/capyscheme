;; Pattern views over the TreeIL intermediate representation.
;;
;; The TreeIL term records live in (boot expand): a family of record types
;; rooted at &term.  This library has two jobs:
;;
;;   * re-export the record interface (type tags, predicates, accessors and
;;     constructors) so compiler passes can build and inspect terms without
;;     depending on the boot module directly;
;;
;;   * attach an srfi-257 match pattern to each record type so that `match'
;;     can destructure terms directly, e.g. a <let> term matches
;;
;;       (~let src style ids lhs rhs body)
;;
;;     with @var{src} bound to the source location, @var{style} to the
;;     binding kind, and the remaining variables to the corresponding
;;     record fields.
;;
;; The pattern variables follow the field order of the underlying record
;; exactly. Renaming a field here would silently change what every
;; `match' clause in the compiler sees, so the field names are part of the
;; stable interface.

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

    &global-ref
    global-ref?
    global-ref-key
    global-ref-name
    global-ref-slot
    make-global-ref

    &global-set
    global-set?
    global-set-key
    global-set-name
    global-set-slot
    global-set-value
    make-global-set

    &linklet
    linklet-term?
    linklet-term-name
    linklet-term-imports
    linklet-term-exports
    linklet-term-slots
    linklet-term-body
    linklet-term-meta
    make-linklet-term

    ~term
    ~lref
    ~lset
    ~if
    ~let
    ~fix
    ~receive
    ~application
    ~primcall
    ~primref
    ~proc
    ~void
    ~constant
    ~values
    ~sequence
    ~global-ref
    ~global-set
    ~linklet
    ~wcm)
  (import (capy) (srfi 257))

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
    (~if src test then else)
    if?
    (src term-src)
    (test if-test)
    (then if-then)
    (else if-else))

  ;; <let> covers `let', `let*' and the `letrec' variants, distinguished
  ;; by `style'.  <fix> is a letrec where the right-hand sides are already
  ;; known lambdas, and <receive> binds the values returned by a producer.
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
    primref?
    (src term-src)
    (name primref-prim))

  (define-record-match-pattern
    (~proc src args body meta ids)
    proc?
    (src term-src)
    (args proc-args)
    (body proc-body)
    (meta proc-meta)
    (ids proc-ids))

  (define-record-match-pattern
    (~constant src value)
    constant?
    (src term-src)
    (value constant-value))

  (define-record-match-pattern
    (~void src)
    void?
    (src term-src))

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
    (~global-ref src key name slot)
    global-ref?
    (src term-src)
    (key global-ref-key)
    (name global-ref-name)
    (slot global-ref-slot))

  (define-record-match-pattern
    (~global-set src key name slot value)
    global-set?
    (src term-src)
    (key global-set-key)
    (name global-set-name)
    (slot global-set-slot)
    (value global-set-value))

  (define-record-match-pattern
    (~linklet src name imports exports slots body meta)
    linklet-term?
    (src term-src)
    (name linklet-term-name)
    (imports linklet-term-imports)
    (exports linklet-term-exports)
    (slots linklet-term-slots)
    (body linklet-term-body)
    (meta linklet-term-meta))

  (define-record-match-pattern
    (~wcm src key mark result)
    wcm?
    (src term-src)
    (key wcm-key)
    (mark wcm-mark)
    (result wcm-result)))
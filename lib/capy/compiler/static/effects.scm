;;; What the whole-program shaker knows about the system, as tables.
;;;
;;; The shaker (capy compiler static shake) decides which top-level bindings of a linked program
;;; can be dropped. For that it has to know which operations have no effect, which only change
;;; their first argument, and which procedures of the libraries behave like one of those.
;;;
;;; Two kinds of table
;;; ------------------
;;;   * Tables of PRIMITIVES (`pure-prims`, `mutator-prims`, `fresh-prims`,
;;;     `expander-state-prims`) are keyed by the name of a variable of `(#%capy)`. A reference to
;;;     one is a reference to the native primitive, whatever a program calls its own variables,
;;;     so the name is enough.
;;;
;;;   * Tables of LIBRARY PROCEDURES (`library-mutators`, `constructor-procs`, `raising-procs`,
;;;     `formatter-procs`, `parameter-makers` and `record-factories`) are keyed by the name of a
;;;     binding of a linklet. A name alone is not an identity: a program (or a library like
;;;     `(capy intrusive dlist)`, which defines its own `make-list`) may define a procedure
;;;     that is called `make-list` or `add-hook!` and does something else. These tables
;;;     are therefore only consulted through `system-ref` and `system-binding?`, which look at
;;;     the owner of the binding too and only trust the linklets of the runtime.

(library (capy compiler static effects)
  (export pure-prims
    mutator-prims
    fresh-prims
    expander-state-prims
    library-mutators
    constructor-procs
    raising-procs
    formatter-procs
    parameter-makers
    record-factories
    system-owner?
    system-ref
    system-binding?
    effect-table-names)
  (import (rnrs)
    (rnrs hashtables))


  ;; The heads of the keys of the linklets that belong to the runtime: the boot linklets
  ;; (`(boot ...)`), the core libraries (`(core ...)`) and the standard ones (`(rnrs ...)`). A
  ;; program is `(%program <file>)` and any other library is the user's.
  (define system-owner-heads '(boot core rnrs scheme))

  (define (system-owner? owner)
    (and (pair? owner) (memq (car owner) system-owner-heads) #t))

  ;; The entry of TABLE for the binding ID of the linklet OWNER, #f when there is none or when
  ;; OWNER is not the runtime's.
  (define (system-ref table owner id)
    (and (system-owner? owner) (hashtable-ref table id #f)))

  (define (system-binding? names owner id)
    (and (system-owner? owner) (memq id names) #t))

  (define (symbol-table syms)
    (let ([t (make-eq-hashtable)])
      (for-each (lambda (s) (hashtable-set! t s #t)) syms)
      t))

  ;; Primitives with no effect when the result is unused.
  (define pure-prims
    (symbol-table
      '(cons list list* cons* vector make-vector make-string string make-bytevector bytevector
        string-append substring string-copy vector-copy bytevector-copy append reverse list-copy
        vector->list list->vector string->list list->string string->symbol symbol->string
        string->number number->string string-length vector-length bytevector-length
        string-ref vector-ref bytevector-u8-ref list-tail list-ref length list?
        car cdr caar cadr cdar cddr caddr cdddr cadddr
        pair? null? not eq? eqv? equal? boolean? symbol? string? char? number? integer? rational?
        real? complex? exact? inexact? procedure? vector? bytevector? tuple? variable? eof-object?
        zero? positive? negative? odd? even? + - * / = < > <= >= quotient remainder modulo
        abs min max expt exact inexact exact->inexact inexact->exact floor ceiling round truncate
        sqrt char->integer integer->char char=? char<? char>? char<=? char>=?
        memq memv assq assv assoc member
        values void unspecified unspecified?
        make-tuple tuple tuple-ref tuple-size
        make-fluid make-thread-local-fluid fluid-ref fluid? make-variable variable-ref
        variable-bound? make-box
        make-core-hash-eq make-core-hash-equal make-core-hash-eqv make-core-hash-string
        make-core-hashtable make-weak-core-hashtable core-hash-ref core-hash-contains?
        core-hash-size core-hash->list core-hash->alist core-hash-copy core-hashtable?
        make-core-persistent-map-eq make-core-persistent-map-equal make-core-persistent-map-eqv
        make-core-persistent-set-eq make-core-persistent-set-equal make-core-persistent-set-eqv
        make-weak-mapping make-ephemeron make-mutex make-syntax-transformer make-syntax
        make-linklet-instance make-class make-generic make-condition
        class? class-of instance? instance-name
        symbol-hash string-hash eq-hash equal-hash
        string=? string<? string>? string<=? string>=? string-null?
        gensym generate-uid make-uuid
        procedure-properties procedure-property
        current-library-registry root-library-registry
        %builtin-class generic-methods method-body %make-method-body class-precedence-list
        char-general-category host-os host-arch host-family logand logior logxor ash hash
        pointer-address pointer? $continuation-marks-markss continuation-mark-set-first
        continuation-marks? current-continuation-marks $winders keyword->symbol keyword?
        fixnum-width fixnum? instance-symbols syntax-expression syntax? syntax-module
        syntax-sourcev $format list-transpose* %null-continuation
        char-numeric? bytevector-ref %utf8->string list-transpose+ library-instance instance-ref instance-variable instance-meta instance-syntax-ref
        instance-syntax-symbols registry-resolver program-arguments bytevector->list syscall:isatty registry-envs
        registry-lazy-envs
        record-type-descriptor? record-constructor-descriptor? record? record-rtd rtd-name rtd-parent
        rtd-uid rtd-sealed? rtd-opaque? rtd-fields rtd-field-count rtd-printer rtd-ancestor?
        rcd-rtd rcd-parent rcd-protocol
        make-record-type-descriptor make-record-constructor-descriptor)))

  ;; Primitives that change their first argument and nothing else.
  (define mutator-prims
    (let ([t (make-eq-hashtable)])
      (for-each (lambda (p) (hashtable-set! t (car p) (cdr p)))
        '((set-car! . 0) (set-cdr! . 0) (vector-set! . 0) (vector-fill! . 0) (string-set! . 0)
          (bytevector-u8-set! . 0) (tuple-set! . 0) (fluid-set! . 0) (variable-set! . 0)
          (slot-set! . 0) (instance-set! . 0) (instance-slot-set! . 0)
          (core-hash-put! . 0) (core-hash-set! . 0) (core-hash-delete! . 0)
          (core-hash-remove! . 0) (core-hash-clear! . 0)
          (set-procedure-properties! . 0) (set-procedure-property! . 0)
          (set-rtd-printer! . 0) (instance-ensure-variable! . 0) (add-method! . 0) (%mark-boot-slot-protocol! . 0)
          (r6rs:bytevector-copy! . 2)))
      t))

  (define fresh-prims
    (symbol-table
      '(cons list vector make-vector make-string string make-bytevector bytevector
        string-append substring string-copy vector-copy bytevector-copy list-copy
        make-tuple tuple make-fluid make-thread-local-fluid make-variable make-box
        make-core-hash-eq make-core-hash-equal make-core-hash-eqv make-core-hash-string
        make-core-hashtable make-weak-core-hashtable make-core-persistent-map-eq
        make-core-persistent-map-equal make-core-persistent-map-eqv make-class make-generic
        make-linklet-instance make-mutex make-weak-mapping make-ephemeron
        make-record-type-descriptor make-record-constructor-descriptor)))

  ;; Primitives whose effect only the loader and the expander can observe: it changes what
  ;; they do when they run. 
  (define expander-state-prims '(set-registry-resolver! install-expander-hook!))

  ;; Procedures of the libraries that mutate an argument. The key is the name of the binding
  ;; and the index of the argument that is the object.
  (define library-mutators
    (let ([t (make-eq-hashtable)])
      (for-each (lambda (p) (hashtable-set! t (car p) (cdr p)))
        '((hashtable-set! . 0) (hashtable-update! . 0) (hashtable-update!/default . 0)
          (hashtable-delete! . 0) (hashtable-clear! . 0) (hashtable-copy! . 0)
          (add-method! . 0) (set-record-printer! . 0)
          (add-hook! . 0) (remove-hook! . 0)
          (env-add! . 0) (env-define! . 0) (env-export! . 0) (set-setter! . 0)))
      t))

  ;; Procedures of the libraries that only make a new object (a record type, a table, an
  ;; enumeration, a parameter, ...), and so have no effect, whatever they are called with.
  (define constructor-procs
    (symbol-table
      '(record-constructor record-predicate record-accessor record-mutator
        make-record-type
        make-enumeration enum-set-constructor enum-set-universe make-parameter
        make-eq-hashtable make-eqv-hashtable make-equal-hashtable make-hashtable
        make-string-hashtable make-condition-type make-list list-copy)))

  ;; Procedures that signal an error: a call that is not used did nothing but fail, like a
  ;; primitive applied to the wrong arguments, so it counts as no effect.
  (define raising-procs
    (symbol-table '(assertion-violation error raise syntax-violation)))

  ;; Procedures that format into a string when their first argument is #f.
  (define formatter-procs '(format $format))

  ;; Procedures that make a parameter: calling the result with no argument reads it.
  (define parameter-makers '(make-parameter))

  ;; Procedures that make the procedures of a record. A call of a constructor, a predicate or an
  ;; accessor has no effect, a mutator changes its first argument.
  (define record-factories
    (let ([t (make-eq-hashtable)])
      (for-each (lambda (p) (hashtable-set! t (car p) (cdr p)))
        '((record-constructor . pure) (record-predicate . pure) (record-accessor . pure)
          (record-mutator . mutator)))
      t))

  ;; Every name the tables mention, for the check against the system.
  (define (effect-table-names)
    (define (keys t) (vector->list (hashtable-keys t)))
    (list
      (cons 'primitive
        (append (keys pure-prims) (keys mutator-prims) (keys fresh-prims) expander-state-prims))
      (cons 'library
        (append (keys library-mutators) (keys constructor-procs) (keys raising-procs)
          formatter-procs parameter-makers (keys record-factories))))))

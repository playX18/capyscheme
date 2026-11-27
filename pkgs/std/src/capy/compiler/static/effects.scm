;;; What the whole-program shaker knows about the system, as tables.
;;;
;;; The shaker (capy compiler static shake) decides which top-level bindings of a linked program
;;; can be dropped. For that it has to know which operations have no effect, which only change
;;; their first argument, and which procedures of the libraries behave like one of those.
;;;
;;; Two kinds of table
;;; ------------------
;;;   * Tables of PRIMITIVES (`pure-prims`, `mutator-prims`, `fresh-prims`,
;;;     `expander-state-prims`) are not written here. They are built from `(primitive-effects)`,
;;;     the effect of each native primitive as the Rust runtime defines it (see
;;;     crates/capy/src/runtime/linklet/effects.rs), which is the one source. They are keyed by
;;;     the name of a variable of `(#%capy)`. A reference to one is a reference to the native
;;;     primitive, whatever a program calls its own variables, so the name is enough. A primitive
;;;     that is not in the table is impure.
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
    (rnrs hashtables)
    (only (capy) primitive-effects))


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

  ;; The primitives, from the table of the runtime (`primitive-effects`, crates/capy/src/runtime/
  ;; linklet/effects.rs): the one place their effects are written down. An entry is
  ;; `(name . effect)`, where the effect is `pure`, `fresh`, `(mutates . index)` or
  ;; `expander-state`.
  (define primitive-entries (primitive-effects))

  (define (entries-named pred)
    (let loop ([entries primitive-entries] [out '()])
      (cond
        [(null? entries) (reverse out)]
        [(pred (cdr (car entries))) (loop (cdr entries) (cons (car (car entries)) out))]
        [else (loop (cdr entries) out)])))

  (define (entries-where pick)
    (let ([t (make-eq-hashtable)])
      (for-each
        (lambda (entry)
          (let ([v (pick (cdr entry))])
            (if v (hashtable-set! t (car entry) v))))
        primitive-entries)
      t))

  ;; Primitives with no effect when the result is unused: the pure ones, and the ones that make
  ;; a new object.
  (define pure-prims
    (entries-where (lambda (e) (and (memq e '(pure fresh)) #t))))

  ;; Primitives that change one argument and nothing else, with the index of the argument.
  (define mutator-prims
    (entries-where (lambda (e) (and (pair? e) (eq? (car e) 'mutates) (cdr e)))))

  ;; Primitives that make a new object that nothing else refers to.
  (define fresh-prims
    (entries-where (lambda (e) (eq? e 'fresh))))

  ;; Primitives whose effect only the loader and the expander can observe: it changes what
  ;; they do when they run.
  (define expander-state-prims
    (entries-named (lambda (e) (eq? e 'expander-state))))

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
  ;; `record-type-rtd` and `%record-field-offset` only read a record type: `define-record-type`
  ;; binds their results to its hidden "%rec" variables, which must not keep an unused type alive.
  (define constructor-procs
    (symbol-table
      '(record-constructor record-predicate record-accessor record-mutator
        make-record-type record-type-rtd %record-field-offset
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

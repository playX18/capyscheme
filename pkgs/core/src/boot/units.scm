(|#%require| (boot prim) (boot control) (boot records) (boot conditions) (boot violations) (boot raise) (boot exceptions) (boot expand) (boot interpreter) (boot enums) (boot sys) (boot osdep) (boot ports-core) (boot transcoder) (boot binary-ports) (boot textual-ports) (boot custom-ports) (boot file-ports) (boot console-ports) (boot port-api) (boot print) (boot format) (boot log) (boot match-syntax) (boot environments))
(|#%provide| all-defined)

;;; Compilation units: the builder psyntax uses to make linklet terms.
;;;
;;; An opening form of a file (a `library`, the `|#%require|` header of a boot file, the
;;; first form of a script) opens a unit, psyntax expands the forms of the unit with it
;;; current, and closing the unit wraps the expanded body in a linklet term. A unit holds
;;;
;;;   kind     library | boot | program | namespace | scratch
;;;   key      what the unit defines: the library name, `(boot <file>)`, or the key of the
;;;            namespace
;;;   env      the expansion environment the unit expands in
;;;   parent   for a scratch unit, the unit whose counter it uses
;;;   slots    the slot variable (lvar) of every variable the body refers to, by the
;;;            linklet that owns the variable: an import slot of the group of its key,
;;;            or a slot of the unit when the unit itself owns it
;;;   assigned the own names that are assigned (`set!`)
;;;   provide  the names of the `|#%provide|` form of a boot file, and whether it said
;;;            `all-defined`
;;;   body     the expansions of the forms of the unit so far a file driver has seen, newest first
;;;
;;; The expander asks the unit for a slot when it builds a global reference, assignment or
;;; definition (`unit-binding-slot`, `unit-definition-slot!`), from the `(key . sym)` the
;;; environment binding says its variable is owned by (see `env-origin`). The terms it
;;; builds are `global-ref` and `global-set` on those slots. Nothing is resolved after the
;;; fact, except by `unit-close!`, which can use what the whole body shows:
;;;
;;; - A unit of kind `library` exports what the export spec of the library says, by the
;;;   internal names (a renamed export is the internal name of the definition). A unit of
;;;   kind `boot` exports what its `|#%provide|` form lists, or every definition for
;;;   `all-defined`. The definitions that are not exported have a variable of the body
;;;   alone (a `let` around the body), which the compiler removes when it does not escape.
;;;   There is one exception, as in Racket: a library that defines syntax also exports its
;;;   other definitions from the linklet, because a macro can expand into references to
;;;   them in other units, and those references are made on the variables of the instance.
;;;   What importers see is the export table of the decl in the meta of the linklet, which is
;;;   exactly the export spec. A `program` keeps exporting all its definitions.
;;;
;;; - A procedure that a library defines, that is not exported, defined once, and never
;;;   assigned does not need its variable inside the body. It is bound by a `fix` around
;;;   the body, references to it in the body are references to the binding (so the
;;;   compiler calls known procedures directly), and its value is also stored in the
;;;   variable of the instance where the definition stands.
;;; - A unit of kind `program` or `namespace` runs in a namespace. It has no exports.
;;; - A unit of kind `scratch` is opened to expand code that is run right away while
;;;   expanding (the right-hand side of a `define-syntax`, `eval-when` code). It is never
;;;   closed, its slots only make the terms well formed: code run while expanding finds
;;;   variables by the key and name of the term (`unit-variable`).

(define current-unit (make-parameter #f))

;; The units that were closed while a file is expanded, newest first, in a box. A
;; library form closes its unit into it, the unit of the file takes the rest.
(define current-unit-sink (make-parameter #f))

;;; The record
;;;
;;; The fields are `kind`, `key`, `env`, `parent` (see above), `groups` (the import groups, newest
;;; first: a vector of the key of the linklet, the `(sym . slot)` pairs newest first, and the eq
;;; table sym -> slot), `own` (eq table: sym -> export slot), `own-order` (the own names in the
;;; order their slot was made, newest first), `assigned`, `counter` (for the unique names),
;;; `provide` (the names a `|#%provide|` form lists), `provide-all?` (whether a `|#%provide|`
;;; form says `all-defined`), `body` and `forward-refs` (`(sym . source)` for each reference or
;;; assignment to a name that nothing binds when it is expanded, newest first: a name that is
;;; defined later in the unit is fine, the others are reported when the unit is closed) and
;;; `unresolved` (`(interface . name)` for each import of a library that does not exist, newest
;;; first: the interface is the empty one the import stands for, NAME how people write the
;;; library; the unbound names of the unit may come from it, see `unbound-entries`).

(define &unit
  (let* ([rtd (make-record-type-descriptor '&unit #f #f #f #f
                '#((immutable kind) (immutable key) (immutable env) (mutable groups)
                   (immutable own) (mutable own-order) (mutable assigned) (mutable counter)
                   (immutable parent) (mutable provide) (mutable provide-all?) (mutable body)
                   (mutable forward-refs) (mutable unresolved)))]
         [rcd (make-record-constructor-descriptor rtd #f #f)])
    (make-record-type '&unit rtd rcd)))

(define %make-unit (record-constructor (record-type-rcd &unit)))
(define unit? (record-predicate (record-type-rtd &unit)))
(define unit-kind (record-accessor (record-type-rtd &unit) 0))
(define unit-key (record-accessor (record-type-rtd &unit) 1))
(define unit-env (record-accessor (record-type-rtd &unit) 2))
(define %unit-groups (record-accessor (record-type-rtd &unit) 3))
(define %unit-set-groups! (record-mutator (record-type-rtd &unit) 3))
(define %unit-own (record-accessor (record-type-rtd &unit) 4))
(define %unit-own-order (record-accessor (record-type-rtd &unit) 5))
(define %unit-set-own-order! (record-mutator (record-type-rtd &unit) 5))
(define %unit-assigned (record-accessor (record-type-rtd &unit) 6))
(define %unit-set-assigned! (record-mutator (record-type-rtd &unit) 6))
(define %unit-counter (record-accessor (record-type-rtd &unit) 7))
(define %unit-set-counter! (record-mutator (record-type-rtd &unit) 7))
(define unit-parent (record-accessor (record-type-rtd &unit) 8))
(define unit-provide (record-accessor (record-type-rtd &unit) 9))
(define %unit-set-provide! (record-mutator (record-type-rtd &unit) 9))
(define unit-provide-all? (record-accessor (record-type-rtd &unit) 10))
(define %unit-set-provide-all! (record-mutator (record-type-rtd &unit) 10))
(define %unit-body (record-accessor (record-type-rtd &unit) 11))
(define %unit-set-body! (record-mutator (record-type-rtd &unit) 11))
(define %unit-forward-refs (record-accessor (record-type-rtd &unit) 12))
(define %unit-set-forward-refs! (record-mutator (record-type-rtd &unit) 12))
(define unit-unresolved (record-accessor (record-type-rtd &unit) 13))
(define %unit-set-unresolved! (record-mutator (record-type-rtd &unit) 13))

;; Records that the empty interface IFACE stands for the library NAME (a string) that U
;; imports and that does not exist.
(define (unit-note-unresolved! u iface name)
  (if (not (assq iface (unit-unresolved u)))
    (%unit-set-unresolved! u (cons (cons iface name) (unit-unresolved u)))))

;; Whether IFACE is an interface that stands for a library that does not exist, for U.
(define (unit-unresolved-interface? u iface)
  (->bool (assq iface (unit-unresolved u))))

;; Whether U runs in a namespace.
(define (unit-namespace? u) (namespace? (unit-env u)))

;; A new unit. Scratch units take a PARENT, whose counter they use.
(define (make-unit kind key env . parent)
  (let ([u (%make-unit kind key env '() (make-core-hash-eq) '() '() 0
             (if (pair? parent) (car parent) #f) '() #f '() '() '())])
    ; The variables of a namespace are the first import group.
    (if (namespace? env)
      (unit-group! u key))
    u))

;; The unit for compiling in ENV, opened by whoever expands the file. KEY is the
;; key of the file for a boot unit. A script or an `eval` expands in a namespace.
(define (make-file-unit env key mode)
  (cond
    [(namespace? env) (make-unit (if (eq? mode 'e) 'namespace 'program) (env-key env) env)]
    [else (make-unit 'boot (or key (env-key env)) env)]))

;; Records the names a `|#%provide|` form lists
(define (unit-provide! u specs)
  (%unit-set-provide! u
    (append (unit-provide u)
      (let loop ([specs specs])
        (cond
          [(null? specs) '()]
          [(symbol? (car specs))
            (if (eq? (car specs) 'all-defined)
              (begin
                (%unit-set-provide-all! u #t)
                (loop (cdr specs)))
              (cons (car specs) (loop (cdr specs))))]
          [else (loop (cdr specs))])))))

;;; Names created while expanding

(define (unit-next-id! u)
  (cond
    [(not u) (env-generate-unique-id!)]
    [(unit-parent u) (unit-next-id! (unit-parent u))]
    [(unit-namespace? u) (env-generate-unique-id! (unit-env u))]
    [else
      (let ([n (%unit-counter u)])
        (%unit-set-counter! u (+ n 1))
        n)]))

;; A fresh symbol `<id>-<hash>-<n>`, where HASH is `equal-hash` of the key of U.
(define (unit-gensym u id)
  (let ([key (if u (unit-key u) (env-key (expansion-env)))])
    (string->symbol
      (string-append id "-" (number->string (equal-hash key)) "-"
        (number->string (unit-next-id! u))))))

;; The unique name `#(key n)` of a mark or label
(define (unit-unique u)
  (if u
    (vector (unit-key u) (unit-next-id! u))
    (vector '(capy) (gensym "id"))))

;;; Slots

(define (unit-group! u key)
  (let loop ([groups (%unit-groups u)])
    (cond
      [(null? groups)
        (let ([group (vector key '() (make-core-hash-eq))])
          (%unit-set-groups! u (cons group (%unit-groups u)))
          group)]
      [(equal? (vector-ref (car groups) 0) key) (car groups)]
      [else (loop (cdr groups))])))

;; The slot of the variable SYM of the linklet KEY, an import of U.
(define (unit-import-slot! u key sym)
  (let* ([group (unit-group! u key)]
         [table (vector-ref group 2)])
    (or (core-hash-ref table sym #f)
      (let ([slot (unit-gensym u (symbol->string sym))])
        (core-hash-put! table sym slot)
        (vector-set! group 1 (cons (cons sym slot) (vector-ref group 1)))
        slot))))

;; The export slot of the variable SYM that U defines.
(define (unit-own-slot! u sym)
  (let ([own (%unit-own u)])
    (or (core-hash-ref own sym #f)
      (let ([slot (unit-gensym u (symbol->string sym))])
        (core-hash-put! own sym slot)
        (%unit-set-own-order! u (cons sym (%unit-own-order u)))
        slot))))

;; The slot a term needs to refer to SYM as seen from KEY (the key of the environment
;; the identifier is bound in) or from its public
;; interface when PUBLIC? is true. The result is a list
;;
;;   (key sym slot)
;;
;; where `(key . sym)` names the variable in the linklet that owns it. The owner is in
;; the environment binding: its origin. A name that
;; nothing binds is a forward reference to a definition of U (it is checked when U is
;; closed), or a variable of the namespace of U. SRC, the source of the reference when it is
;; known, is what the report of an unbound name points at.
(define (unit-binding-slot u key sym public? . src)
  (let* ([env (unit-env u)]
         [base (if (or (not key) (equal? key (env-key env))) env (global-environment key))]
         [scope (if public? (env-public-interface base) base)]
         [origin (or (env-origin scope sym)
                   (and key (not (equal? key (env-key env))) (cons key sym)))]
         [own? (and origin (not (unit-namespace? u)) (equal? (car origin) (unit-key u)))])
    (cond
      ;; a name of `(#%capy)` that a boot file defines is the primitive's variable (see
      ;; `unit-definition-slot!`), also where the running system binds it to the file itself
      [(and (or own? (not origin)) (boot-primitive-definition? u sym))
        (list '(|#%capy|) sym (unit-import-slot! u '(|#%capy|) sym))]
      [own? (list (car origin) (cdr origin) (unit-own-slot! u (cdr origin)))]
      [origin (list (car origin) (cdr origin) (unit-import-slot! u (car origin) (cdr origin)))]
      [(unit-namespace? u) (list (unit-key u) sym (unit-import-slot! u (unit-key u) sym))]
      [else
        (if (memq (unit-kind u) '(library boot))
          (%unit-set-forward-refs! u
            (cons (cons sym (and (pair? src) (car src))) (%unit-forward-refs u))))
        (list (unit-key u) sym (unit-own-slot! u sym))])))

(define (unit-assignment-slot u key sym public? . src)
  (let ([found (apply unit-binding-slot u key sym public? src)])
    (if (and (eq? (caddr found) (core-hash-ref (%unit-own u) (cadr found) #f))
         (not (memq (cadr found) (%unit-assigned u))))
      (%unit-set-assigned! u (cons (cadr found) (%unit-assigned u))))
    found))

;; The variable SYM that U defines. A boot file that defines a name of `(#%capy)` assigns the
;; variable of the primitive instead of defining its own: that is how `boot/prim.scm` and the
;; files after it supply the procedures the runtime has only a variable for (`memq`, ...), and
;; how references to the name, from any file, reach the definition. Such a definition is not
;; exported and does not rebind the name, which stays the `(#%capy)` one.
(define (unit-definition-slot! u sym)
  (cond
    [(boot-primitive-definition? u sym)
      (list '(|#%capy|) sym (unit-import-slot! u '(|#%capy|) sym))]
    [else
      (env-own-variable! (unit-env u) sym (unit-key u))
      (if (unit-namespace? u)
        (list (unit-key u) sym (unit-import-slot! u (unit-key u) sym))
        (list (unit-key u) sym (unit-own-slot! u sym)))]))

;; Whether a definition of SYM in U assigns the `(#%capy)` variable SYM: U is a boot unit, SYM
;; is a name of `(#%capy)`, and the `|#%provide|` form of U does not list it (a listed name is
;; exported, as `call/cc` by `boot/control.scm`).
(define (boot-primitive-definition? u sym)
  (and (eq? (unit-kind u) 'boot)
    (not (memq sym (unit-provide u)))
    (capy-primitive-variable sym)
    #t))

;; The variable SYM of the `(#%capy)` instance, or #f.
(define (capy-primitive-variable sym)
  (let ([prims (library-instance (current-library-registry) '(|#%capy|))])
    (and prims (instance-variable prims sym))))


;;; Variables of code run while expanding

(define (unit-variable key sym)
  (let ([u (current-unit)])
    (cond
      [(and u (equal? key (unit-key u)))
        (or (env-local-variable (unit-env u) sym)
          (env-own-variable! (unit-env u) sym key))]
      [(let ([inst (library-instance (current-library-registry) key)])
         (and inst (instance-variable inst sym)))]
      [else
        (let ([env (global-environment key)])
          (or (env-local-variable env sym)
            (env-ensure-local-variable! env sym)))])))

;;; Running the expander in a unit

;; Calls THUNK with U current, and the environment of U the expansion environment.
(define (call-with-unit u thunk)
  (call-with-expansion-env (unit-env u)
    (lambda ()
      (let ([saved #f])
        (dynamic-wind
          (lambda ()
            (set! saved (current-unit))
            (current-unit u))
          thunk
          (lambda ()
            (current-unit saved)))))))

;; Calls THUNK with a scratch unit current, see above. Terms built by THUNK are not
;; part of any linklet.
(define (call-with-scratch-unit thunk)
  (let* ([parent (current-unit)]
         [env (if parent (unit-env parent) (expansion-env))])
    (call-with-unit
      (make-unit 'scratch (if parent (unit-key parent) (env-key env)) env parent)
      thunk)))

;;; Closing

;; The forms of the term T that are not void: the forms of nested sequences, in order.
(define (top-level-forms t)
  (cond
    [(sequence? t)
      (append (top-level-forms (sequence-head t)) (top-level-forms (sequence-tail t)))]
    [(void? t) '()]
    [else (list t)]))

;; Rebuilds `t` with `f` applied to each of its child terms, in order.
(define (term-map-children t f)
  (define src (term-src t))
  (cond
    [(or (constant? t) (void? t) (lref? t) (primref? t) (global-ref? t))
      t]
    [(lset? t)
      (make-lset src (lset-name t) (lset-sym t) (f (lset-value t)))]
    [(global-set? t)
      (let ([value (f (global-set-value t))])
        (make-global-set src (global-set-key t) (global-set-name t) (global-set-slot t) value))]
    [(if? t)
      (let* ([test (f (if-test t))]
             [then (f (if-then t))]
             [alt (f (if-else t))])
        (make-if src test then alt))]
    [(let? t)
      (let* ([rhs (map-in-order f (let-rhs t))]
             [body (f (let-body t))])
        (make-let src (let-style t) (let-ids t) (let-lhs t) rhs body))]
    [(fix? t)
      (let* ([rhs (map-in-order f (fix-rhs t))]
             [body (f (fix-body t))])
        (make-fix src (fix-ids t) (fix-lhs t) rhs body))]
    [(receive? t)
      (let* ([producer (f (receive-producer t))]
             [consumer (f (receive-consumer t))])
        (make-receive src (receive-ids t) (receive-vars t) producer consumer))]
    [(application? t)
      (let* ([operator (f (application-operator t))]
             [operands (map-in-order f (application-operands t))])
        (make-application src operator operands))]
    [(primcall? t)
      (make-primcall src (primcall-prim t) (map-in-order f (primcall-args t)))]
    [(proc? t)
      (make-proc src (proc-args t) (f (proc-body t)) (proc-meta t) (proc-ids t))]
    [(linklet-term? t)
      (make-linklet-term src (linklet-term-name t) (linklet-term-imports t)
        (linklet-term-exports t) (linklet-term-slots t) (f (linklet-term-body t))
        (linklet-term-meta t))]
    [(values? t)
      (make-values src (map-in-order f (values-values t)))]
    [(sequence? t)
      (let* ([head (f (sequence-head t))]
             [tail (f (sequence-tail t))])
        (make-sequence src head tail))]
    [(wcm? t)
      (let* ([key (f (wcm-key t))]
             [mark (f (wcm-mark t))]
             [result (f (wcm-result t))])
        (make-wcm src key mark result))]
    [(unbound-variable-term? t)
      (let ([value (unbound-variable-term-value t)])
        (make-unbound-variable-term src (unbound-variable-term-name t)
          (unbound-variable-term-key t) (and value (f value))))]
    [(syntax-error-term? t)
      (make-syntax-error-term src (syntax-error-term-diagnostic t)
        (map-in-order f (syntax-error-term-children t)))]
    [else (error 'term-map-children "unknown TreeIL term" t)]))

(define (map-in-order f ls)
  (if (null? ls)
    '()
    (let* ([x (f (car ls))]
           [rest (map-in-order f (cdr ls))])
      (cons x rest))))

;;; Terms as data
;;;
;;; A term is a record, which does not survive a FASL round trip. `term->datum` turns it into
;;; plain data and `datum->term` back into the term, so the machine-independent form of a
;;; compiled library can be kept in a `.wpo` file (see `write-wpo!` in eval.scm). The datum of
;;; a term is a vector `#(tag src field ...)`, with the fields in the order of the shapes
;;; below: a `t` field is a term, an `l` field a list of terms and a `v` field is kept as it
;;; is. The names of variables are symbols; the uninterned ones keep their identity inside one
;;; image.

(define term-shapes
  (list
    (list 'lref lref? make-lref (list lref-name lref-sym) '(v v))
    (list 'lset lset? make-lset (list lset-name lset-sym lset-value) '(v v t))
    (list 'if if? make-if (list if-test if-then if-else) '(t t t))
    (list 'let let? make-let (list let-style let-ids let-lhs let-rhs let-body) '(v v v l t))
    (list 'fix fix? make-fix (list fix-ids fix-lhs fix-rhs fix-body) '(v v l t))
    (list 'receive receive? make-receive
      (list receive-ids receive-vars receive-producer receive-consumer) '(v v t t))
    (list 'application application? make-application
      (list application-operator application-operands) '(t l))
    (list 'primcall primcall? make-primcall (list primcall-prim primcall-args) '(v l))
    (list 'primref primref? make-primref (list primref-prim) '(v))
    (list 'constant constant? make-constant (list constant-value) '(v))
    (list 'void void? make-void '() '())
    (list 'proc proc? make-proc (list proc-args proc-body proc-meta proc-ids) '(v t v v))
    (list 'values values? make-values (list values-values) '(l))
    (list 'sequence sequence? make-sequence (list sequence-head sequence-tail) '(t t))
    (list 'wcm wcm? make-wcm (list wcm-key wcm-mark wcm-result) '(t t t))
    (list 'global-ref global-ref? make-global-ref
      (list global-ref-key global-ref-name global-ref-slot) '(v v v))
    (list 'global-set global-set? make-global-set
      (list global-set-key global-set-name global-set-slot global-set-value) '(v v v t))
    (list 'linklet linklet-term? make-linklet-term
      (list linklet-term-name linklet-term-imports linklet-term-exports linklet-term-slots
        linklet-term-body linklet-term-meta)
      '(v v v v t v))))

(define (map2-in-order f xs ys)
  (if (null? xs)
    '()
    (let* ([x (f (car xs) (car ys))]
           [rest (map2-in-order f (cdr xs) (cdr ys))])
      (cons x rest))))

(define (term->datum t)
  (let loop ([shapes term-shapes])
    (cond
      [(null? shapes) (error 'term->datum "unknown TreeIL term" t)]
      [((cadr (car shapes)) t)
        (let ([shape (car shapes)])
          (list->vector
            (cons (car shape)
              (cons (term-src t)
                (map2-in-order
                  (lambda (access kind)
                    (let ([field (access t)])
                      (case kind
                        [(t) (term->datum field)]
                        [(l) (map-in-order term->datum field)]
                        [else field])))
                  (cadddr shape) (list-ref shape 4))))))]
      [else (loop (cdr shapes))])))

(define (datum->term d)
  (let ([shape (assq (vector-ref d 0) term-shapes)])
    (if (not shape)
      (error 'datum->term "unknown TreeIL datum" d))
    (apply (caddr shape)
      (vector-ref d 1)
      (map2-in-order
        (lambda (field kind)
          (case kind
            [(t) (datum->term field)]
            [(l) (map-in-order datum->term field)]
            [else field]))
        (cddr (vector->list d)) (list-ref shape 4)))))

;;; Private variables

;; A unit whose definitions are not all exported binds the private ones with a `let` of fresh
;; variables around its body (`bind-private-variables`). A bundle that links the unit has no
;; instance to hold them, so `hoist-private-variables` is the inverse: it makes them extra
;; exports of the linklet, named by their variable, so the bundle binds them like any other
;; definition.
(define (make-variable-term? t)
  (or (and (primcall? t) (eq? (primcall-prim t) 'make-variable))
    ;; expand-primitives turns the primcall into the application of the primitive
    (and (application? t) (null? (application-operands t))
      (primref? (application-operator t))
      (eq? (primref-prim (application-operator t)) 'make-variable))))

(define (private-variable-let? b)
  (and (let? b) (eq? (let-style b) 'let)
    (let loop ([rhs (let-rhs b)])
      (or (null? rhs)
        (and (make-variable-term? (car rhs))
          (loop (cdr rhs)))))))

(define (hoist-private-variables t)
  (let ([b (linklet-term-body t)])
    (if (private-variable-let? b)
      (make-linklet-term (term-src t) (linklet-term-name t) (linklet-term-imports t)
        (append (linklet-term-exports t) (let-lhs b))
        (append (linklet-term-slots t) (let-lhs b))
        (let-body b)
        (linklet-term-meta t))
      t)))

;; Whether U has anything to run or to tell the namespace: the body has a form, the
;; unit imported something, or it refers to variables.
(define (unit-has-content? u body)
  (or (eq? (unit-kind u) 'library)
    (pair? (top-level-forms body))
    (and (unit-namespace? u) (env-unit-imports? (unit-env u)))
    (and (not (unit-namespace? u)) (pair? (%unit-own-order u)))))

(define (own-definitions u forms)
  (let loop ([forms forms] [out '()])
    (if (null? forms)
      (reverse out)
      (let* ([t (car forms)]
             [slot (and (global-set? t)
                     (equal? (global-set-key t) (unit-key u))
                     (core-hash-ref (%unit-own u) (global-set-name t) #f))])
        (loop (cdr forms)
          (if (and slot (eq? slot (global-set-slot t))) (cons t out) out))))))

;; The definitions of DEFS that `bind-local-definitions` can turn into local `fix` bindings:
;; procedures defined once and never assigned. An exported one is still stored into its
;; variable, so importers see the same value; only references from inside U become local,
;; which lets the optimizer inline them. EXPORTED are the names that must stay global: the
;; exports of a unit that defines syntax, since its macros can expand into assignments of
;; them in other units (the same rule as `unit-knowns`), else '().
(define (localizable-definitions u defs exported)
  (let ([counts (make-core-hash-eq)]
        [exported (symbol-set exported)]
        [assigned (symbol-set (%unit-assigned u))])
    (for-each
      (lambda (t)
        (core-hash-put! counts (global-set-name t)
          (+ 1 (core-hash-ref counts (global-set-name t) 0))))
      defs)
    (let loop ([defs defs] [out '()])
      (cond
        [(null? defs) (reverse out)]
        [else
          (let* ([t (car defs)] [sym (global-set-name t)])
            (loop (cdr defs)
              (if (and (= (core-hash-ref counts sym 0) 1)
                   (proc? (global-set-value t))
                   (not (core-hash-ref exported sym #f))
                   (not (core-hash-ref assigned sym #f)))
                (cons (cons t (unit-gensym u (symbol->string sym))) out)
                out)))]))))

(define (bind-local-definitions body locals)
  (if (null? locals)
    body
    (let ([by-slot (make-core-hash-eq)]
          [ids '()] [lhs '()] [rhs '()])
      (for-each
        (lambda (entry) (core-hash-put! by-slot (global-set-slot (car entry)) entry))
        locals)
      (letrec ([rewrite
                 (lambda (t)
                   (cond
                     [(and (global-ref? t) (core-hash-ref by-slot (global-ref-slot t) #f))
                       => (lambda (entry)
                            (make-lref (term-src t) (global-ref-name t) (cdr entry)))]
                     [(and (global-set? t) (core-hash-ref by-slot (global-set-slot t) #f))
                       => (lambda (entry)
                            (let ([value (rewrite (global-set-value t))])
                              (set! ids (cons (global-set-name t) ids))
                              (set! lhs (cons (cdr entry) lhs))
                              (set! rhs (cons value rhs))
                              (make-global-set (term-src t) (global-set-key t) (global-set-name t)
                                (global-set-slot t)
                                (make-lref (term-src t) (global-set-name t) (cdr entry)))))]
                     [else (term-map-children t rewrite)]))])
        (let ([body (rewrite body)])
          (make-fix #f (reverse ids) (reverse lhs) (reverse rhs) body))))))

(define (unit-import-groups u)
  (reverse
    (map-in-order
      (lambda (group)
        (cons (vector-ref group 0) (map-in-order car (reverse (vector-ref group 1)))))
      (%unit-groups u))))

(define (unit-import-slots u)
  (apply append
    (map-in-order
      (lambda (group) (map-in-order cdr (reverse (vector-ref group 1))))
      (reverse (%unit-groups u)))))

;; Wraps BODY, the expansion of the forms of U, in a linklet term.
(define (unit-close! u body)
  (case (unit-kind u)
    [(library boot) (close-linking-unit u body)]
    [(program namespace)
      (let ([sink (collecting-sink)])
        (if (and sink (lint-calls?) (eq? (unit-kind u) 'program))
          (lint-call-arities! u body (own-definitions u (top-level-forms body)) sink)))
      (make-linklet-term #f (unit-key u)
        (unit-import-groups u) '() (unit-import-slots u) body
        (namespace-meta (unit-env u)))]
    [else (error 'unit-close! "cannot close the unit" (unit-kind u))]))

(define (program-key? key)
  (and (pair? key) (eq? (car key) '%program)))

(define (dedupe-symbols syms)
  (let ([seen (make-core-hash-eq)])
    (let loop ([syms syms] [out '()])
      (cond
        [(null? syms) (reverse out)]
        [(core-hash-ref seen (car syms) #f) (loop (cdr syms) out)]
        [else
          (core-hash-put! seen (car syms) #t)
          (loop (cdr syms) (cons (car syms) out))]))))

;; Whether SYM names a variable that `define-record-type` made for the inline bodies of its
;; procedures (the record type, the tuple index of a field). The name starts with "%rec " (a
;; space, so a program cannot write it without `|...|`). A library exports such a variable
;; from its linklet although the export spec does not list it, and an inline body may refer to
;; it: this is how an accessor of a record type that another library defined inlines there.
;; The decl does not list it (a program cannot import it), and the interface hash covers it
;; through the bodies of the knowns that mention it.
(define (inline-exposed-name? sym)
  (let ([s (symbol->string sym)])
    (and (> (string-length s) 5)
      (string=? (substring s 0 5) "%rec "))))

;; The names of DEFINED (the definitions of U) that its linklet exports, see above.
(define (unit-exported-names u defined)
  (define (only names)
    (let ([names (symbol-set names)])
      (let loop ([syms defined] [out '()])
        (cond
          [(null? syms) (reverse out)]
          [(or (core-hash-ref names (car syms) #f) (inline-exposed-name? (car syms)))
            (loop (cdr syms) (cons (car syms) out))]
          [else (loop (cdr syms) out)]))))
  (cond
    [(program-key? (unit-key u)) defined]
    [(eq? (unit-kind u) 'library)
      (if (env-defines-macros? (unit-env u))
        defined
        (only (env-export-internals (unit-env u))))]
    [(or (unit-provide-all? u) (null? (unit-provide u))) defined]
    [else (only (unit-provide u))]))

;; Wraps BODY in a `let` that binds the slots of the definitions PRIVATE to fresh variables.
(define (bind-private-variables u body private)
  (if (null? private)
    body
    (make-let #f 'let private
      (map-in-order (lambda (sym) (core-hash-ref (%unit-own u) sym #f)) private)
      (map-in-order (lambda (sym) (make-primcall #f 'make-variable '())) private)
      body)))

;;; Knowns
;;;
;;; What the compiler knows about a variable that a library exports, from its definition.
;;; The knowns of a library are in its decl (`library-decl-knowns`), so the importers of the
;;; library, compiled in other processes or other times, can use them without running it.
;;;
;;;   (constant datum)         the variable is defined once, to an immediate literal, and is
;;;                            never assigned. An importer replaces the reference with DATUM
;;;   (procedure arity-mask)   the variable is defined once, to a `lambda`, and is never
;;;                            assigned. A call of it from an importer is checked against the
;;;                            mask
;;;
;;; A variable is never known when it is assigned or defined twice, and a library that defines
;;; syntax has no knowns: its macros can expand into references to any of its variables, and
;;; assign them. Whether an export is a replacement does not matter to its known.
;;;
;;; Knowns are sound because the knowns are part of the interface hash of the library
;;; (environments.scm): an importer that folded one is compiled again when the library
;;; changes it.

;; Whether the datum X is a literal that can be copied into the code of an importer: one that
;; has no identity.
(define (known-literal? x)
  (or (boolean? x)
    (null? x)
    (char? x)
    (symbol? x)
    (and (exact-integer? x) (<= -1073741824 x) (<= x 1073741823))))

;; The arity mask of the formals FORMALS of a `lambda`.
(define (formals-arity-mask formals)
  (let loop ([formals formals] [n 0] [bit 1])
    (cond
      [(pair? formals) (loop (cdr formals) (+ n 1) (* bit 2))]
      [(null? formals) bit]
      [else (- bit)])))

;; Whether a procedure with the arity mask MASK takes N arguments.
(define (arity-mask-accepts? mask n)
  (let loop ([mask mask] [n n])
    (cond
      [(= n 0) (odd? mask)]
      [else (loop (quotient (- mask (if (odd? mask) 1 0)) 2) (- n 1))])))

;; The knowns of the library of U: `((internal . known) ...)`, for the definitions DEFS (the
;; `global-set` terms of the definitions of U) that the library exports as variables.
(define (unit-knowns u defs)
  (let* ([env (unit-env u)]
         [counts (make-core-hash-eq)]
         [values-table (make-core-hash-eq)]
         ;; computed once: each of them is linear (or worse) in the number of exports, and a
         ;; generated library (c-include) exports thousands of procedures
         [exports (env-library-exports env)]
         [exported (symbol-set (map-in-order cadr exports))]
         [assigned (symbol-set (%unit-assigned u))]
         [_exposed (for-each
                     (lambda (t)
                       (if (inline-exposed-name? (global-set-name t))
                         (core-hash-put! exported (global-set-name t) #t)))
                     defs)]
         [seen (make-core-hash-eq)])
    (for-each
      (lambda (t)
        (let ([sym (global-set-name t)])
          (core-hash-put! counts sym (+ 1 (core-hash-ref counts sym 0)))
          (core-hash-put! values-table sym (global-set-value t))))
      defs)
    (let loop ([exports exports]
               [out '()])
      (if (null? exports)
        (reverse out)
        (let* ([entry (car exports)]
               [sym (cadr entry)]
               [value (core-hash-ref values-table sym #f)]
               [known
                 (and (eq? (caddr entry) 'variable)
                   (not (core-hash-ref seen sym #f))
                   (= (core-hash-ref counts sym 0) 1)
                   (not (core-hash-ref assigned sym #f))
                   value
                   (cond
                     [(and (constant? value) (known-literal? (constant-value value)))
                       (list 'constant (constant-value value))]
                     [(proc? value)
                       (let ([mask (formals-arity-mask (proc-args value))]
                             [body (inline-known-body (unit-key u) sym value exported)])
                         (if body
                           (list 'procedure mask body)
                           (list 'procedure mask)))]
                     [else #f]))])
          (core-hash-put! seen sym #t)
          (loop (cdr exports)
            (if known (cons (cons sym known) out) out)))))))

;; A set of the symbols SYMS, for `core-hash-ref`.
(define (symbol-set syms)
  (let ([table (make-core-hash-eq)])
    (for-each (lambda (sym) (core-hash-put! table sym #t)) syms)
    table))

;;; Inline bodies
;;;
;;; A `procedure` known can carry a third element, the body of the procedure as a datum, when
;;; the procedure is small enough to be copied into its callers: `(procedure mask body)`. An
;;; importer replaces a call of the procedure by a `let` of its formals around a copy of the
;;; body (`splice-inline-call`), which the optimizer then simplifies like any local code. This is
;;; how a small accessor or predicate of one file (`%port-in-ptr`) costs nothing in another.
;;;
;;; The body is kept only when copying it cannot change what it means:
;;;
;;;   - it is a `lambda` with a fixed number of arguments
;;;   - it refers to no variable but its own formals, variables bound inside it, and global
;;;     variables (by key and name, so the importer refers to the same variable)
;;;   - it makes no closure and assigns nothing: copies of it share no state, the way calls of
;;;     the procedure would not
;;;   - its constants have no identity (`known-literal?`), so a copy is `eq?` to the original
;;;   - it does not call itself
;;;
;;; The datum, a vector per term:
;;;
;;;   #(c datum)        constant              #(v)              void
;;;   #(l n)            local reference       #(g key name)     global reference
;;;   #(p prim)         primitive reference   #(pc prim t ...)  primitive call
;;;   #(a t t ...)      application           #(i t t t)        if
;;;   #(s t t)          sequence              #(let t ... t)    let, the right-hand sides first
;;;
;;; and the whole body is `#(arity t)`. A variable is the number of its binding, in the order of
;;; the formals first and then of the `let`s, and has no name: neither the gensym nor the name of
;;; a variable is part of the datum. So the datum is a function of the procedure alone (not of
;;; how many gensyms its unit made before it), and the Rust boot expander, whose variables are
;;; named differently from those of psyntax, makes the same datum from the same source.

;; The largest body, in terms, that is kept as an inline known, and the most formals it can
;; have (so its arity mask is a fixnum). crates/capy/src/expander/boot_knowns.rs has the same.
(define inline-known-max-terms 32)
(define inline-known-max-formals 16)

;; The body datum of the procedure term VALUE, defined as the global NAME of the unit KEY, or
;; #f when it cannot be inlined (see above). EXPORTED is the set (`symbol-set`) of the variables
;; of KEY that other units can refer to: a reference to any other variable of KEY keeps the body out.
(define (inline-known-body key name value exported)
  (call/cc
    (lambda (fail)
      (let ([budget inline-known-max-terms] [count 0])
        (define (number! sym)
          (let ([n count])
            (set! count (+ count 1))
            (cons sym n)))
        (define (spend!)
          (set! budget (- budget 1))
          (if (< budget 0) (fail #f)))
        (define (walk t bound)
          (spend!)
          (cond
            [(constant? t)
              (let ([v (constant-value t)])
                (cond
                  [(eq? v (if #f #f)) (vector 'v)]
                  [(known-literal? v) (vector 'c v)]
                  [else (fail #f)]))]
            [(void? t) (vector 'v)]
            [(lref? t)
              (let ([n (assq (lref-sym t) bound)])
                (if n (vector 'l (cdr n)) (fail #f)))]
            [(global-ref? t)
              (if (and (equal? (global-ref-key t) key)
                    (or (eq? (global-ref-name t) name)
                      (not (core-hash-ref exported (global-ref-name t) #f))))
                (fail #f)
                (vector 'g (global-ref-key t) (global-ref-name t)))]
            [(primref? t) (vector 'p (primref-prim t))]
            [(primcall? t)
              (list->vector
                (cons 'pc (cons (primcall-prim t)
                            (map-in-order (lambda (x) (walk x bound)) (primcall-args t)))))]
            [(application? t)
              (if (list? (application-operands t))
                (list->vector
                  (cons 'a (map-in-order (lambda (x) (walk x bound))
                             (cons (application-operator t) (application-operands t)))))
                (fail #f))]
            ;; `let*`: walking numbers the variables, so the order is fixed (the order the
            ;; arguments of a call are evaluated in is not, and differs between compilers)
            [(if? t)
              (let* ([test (walk (if-test t) bound)]
                     [then (walk (if-then t) bound)]
                     [else (walk (if-else t) bound)])
                (vector 'i test then else))]
            [(sequence? t)
              (let* ([head (walk (sequence-head t) bound)]
                     [tail (walk (sequence-tail t) bound)])
                (vector 's head tail))]
            [(and (let? t) (eq? (let-style t) 'let))
              (let* ([rhs (map-in-order (lambda (x) (walk x bound)) (let-rhs t))]
                     [inner (append (map-in-order number! (let-lhs t)) bound)])
                (let ([body (walk (let-body t) inner)])
                  (list->vector (cons 'let (append rhs (list body))))))]
            [else (fail #f)]))
        (and (proc? value)
          (list? (proc-args value))
          (< (length (proc-args value)) inline-known-max-formals)
          (let* ([formals (map-in-order number! (proc-args value))]
                 [body (walk (proc-body value) formals)])
            (vector (length formals) body)))))))

;; The term of the call of the inline body BODY (see `inline-known-body`) with the argument
;; terms ARGS, at the source SRC, in the unit U, or #f when U cannot refer to a global the body
;; refers to. GLOBAL-SLOT gives the slot of U for a global `(key name)`, or #f. The variables of
;; the body get fresh names, so a unit can splice one body any number of times.
(define (splice-inline-call u body args src global-slot)
  (call/cc
    (lambda (fail)
      (let ([arity (vector-ref body 0)] [count 0] [vars '()])
        ;; binds the next variable number, as `inline-known-body` numbered them
        (define (bind!)
          (let ([sym (unit-gensym u "t")])
            (set! vars (cons (cons count sym) vars))
            (set! count (+ count 1))
            sym))
        (define (build d)
          (case (vector-ref d 0)
            [(c) (make-constant src (vector-ref d 1))]
            [(v) (make-void src)]
            [(l) (make-lref src 't (cdr (assv (vector-ref d 1) vars)))]
            [(g)
              (let ([slot (global-slot (vector-ref d 1) (vector-ref d 2))])
                (if slot
                  (make-global-ref src (vector-ref d 1) (vector-ref d 2) slot)
                  (fail #f)))]
            [(p) (make-primref src (vector-ref d 1))]
            [(pc)
              (make-primcall src (vector-ref d 1)
                (map-in-order build (cddr (vector->list d))))]
            [(a)
              (let ([parts (map-in-order build (cdr (vector->list d)))])
                (make-application src (car parts) (cdr parts)))]
            ;; in order, as `inline-known-body` numbered the variables
            [(i)
              (let* ([test (build (vector-ref d 1))]
                     [then (build (vector-ref d 2))]
                     [else (build (vector-ref d 3))])
                (make-if src test then else))]
            [(s)
              (let* ([head (build (vector-ref d 1))]
                     [tail (build (vector-ref d 2))])
                (make-sequence src head tail))]
            [(let)
              (let* ([parts (cdr (vector->list d))]
                     [rhs (map-in-order build (reverse (cdr (reverse parts))))]
                     [syms (map-in-order (lambda (r) (bind!)) rhs)])
                (let ([body (build (car (reverse parts)))])
                  (make-let src 'let (map-in-order (lambda (s) 't) syms) syms rhs body)))]
            [else (error 'splice-inline-call "unknown inline datum" d)]))
        (and (= arity (length args))
          (let ([syms (let loop ([i 0] [out '()])
                        (if (= i arity) (reverse out) (loop (+ i 1) (cons (bind!) out))))])
            (let ([built (build (vector-ref body 1))])
              (if (null? syms)
                built
                (make-let src 'let (map-in-order (lambda (s) 't) syms) syms args built)))))))))

;; The knowns of the boot unit U, the third element of its `(#%boot key knowns)` meta: the
;; inline bodies of the exported procedures of LOCALS (see `close-linking-unit`), from DEFS, the
;; definitions of U before `fold-imported-knowns` copied the bodies of other units into them.
;; Unlike a library, a boot unit can be loaded three ways: expanded by the Rust boot expander
;; (crates/capy/src/expander/boot_linklet.rs, which makes the same knowns), by psyntax in
;; memory, or from its compiled file. The knowns are those of its source alone, so every way
;; gives the same ones, and so do the stages of the bootstrap.
(define (boot-unit-knowns u defs locals exports)
  (define exported (symbol-set exports))
  (define local (symbol-set (map-in-order (lambda (entry) (global-set-name (car entry))) locals)))
  (let loop ([defs defs] [out '()])
    (if (null? defs)
      (reverse out)
      (let* ([t (car defs)] [sym (global-set-name t)] [value (global-set-value t)])
        (loop (cdr defs)
          (let ([body (and (core-hash-ref exported sym #f)
                        (core-hash-ref local sym #f)
                        (inline-known-body (unit-key u) sym value exported))])
            (if body
              (cons (cons sym (list 'procedure (formals-arity-mask (proc-args value)) body)) out)
              out)))))))

;; The knowns of the imported library KEY, an alist, or '().
(define (imported-knowns key)
  (let ([decl (library-decl-of key)])
    (if decl (library-decl-knowns decl) (env-boot-knowns key))))

;; The knowns of the libraries U imports: `((key . knowns) ...)`, without the libraries that have none.
(define (imported-knowns-tables u)
  (let ([self (unit-key u)])
    (let loop ([groups (unit-import-groups u)] [out '()])
      (cond
        [(null? groups) out]
        [(equal? (car (car groups)) self) (loop (cdr groups) out)]
        [else
          (let ([knowns (imported-knowns (car (car groups)))])
            (loop (cdr groups)
              (if (null? knowns) out (cons (cons (car (car groups)) knowns) out))))]))))

;; The known of the variable the `global-ref` term T refers to, in TABLES, or #f.
(define (known-in-tables tables t)
  (let ([table (assoc (global-ref-key t) tables)])
    (and table
      (let ([found (assq (global-ref-name t) (cdr table))])
        (and found (cdr found))))))

;; BODY, the expansion of the forms of U, with the references to variables that an imported
;; library knows to be constants replaced by the constants. A call of an imported procedure
;; that does not match the arity it is known to have is noted in the log, and left as it is
;; (it raises when it runs).
;;
;; A call of an imported procedure that has an inline body is replaced by a copy of the body
;; (`splice-inline-call`), when every global the body refers to belongs to a library U imports
;; already: inlining never adds a dependency, so it cannot change the order libraries are
;; instantiated in. The copy is rewritten in turn, so the bodies it calls are inlined too, up to
;; `inline-known-depth` levels.
(define inline-known-depth 4)

(define (fold-imported-knowns u body)
  (let* ([tables (imported-knowns-tables u)])
    (define (known-of t) (known-in-tables tables t))
    (define (global-slot key name)
      (let loop ([groups (%unit-groups u)])
        (cond
          [(null? groups) #f]
          [(equal? (vector-ref (car groups) 0) key) (unit-import-slot! u key name)]
          [else (loop (cdr groups))])))
    (define (rewrite t depth)
      (cond
        [(global-ref? t)
          (let ([known (known-of t)])
            (if (and known (eq? (car known) 'constant))
              (make-constant (term-src t) (cadr known))
              t))]
        [(application? t)
          (let* ([operator (application-operator t)]
                 [known (and (global-ref? operator) (known-of operator))]
                 [operands (application-operands t)])
            (if (and known (eq? (car known) 'procedure)
                 (list? operands)
                 (not (arity-mask-accepts? (cadr known) (length operands))))
              (*raw-log* log:debug '(capy) 'knowns
                "call of ~a of ~a with ~a arguments does not match its arity"
                (global-ref-name operator) (global-ref-key operator)
                (length operands)))
            (let ([spliced
                    (and known (eq? (car known) 'procedure) (pair? (cddr known))
                      (> depth 0) (list? operands)
                      (splice-inline-call u (caddr known)
                        (map-in-order (lambda (x) (rewrite x depth)) operands)
                        (term-src t) global-slot))])
              (if spliced
                (rewrite spliced (- depth 1))
                (term-map-children t (lambda (x) (rewrite x depth))))))]
        [else (term-map-children t (lambda (x) (rewrite x depth)))]))
    (if (null? tables)
      body
      (rewrite body inline-known-depth))))

;;; Collecting errors
;;;
;;; `expansion-error-mode` says what the expander does with an error it can recover from:
;;;
;;;   raise     raises it at once. This is the mode of `eval`, the REPL and `environment`
;;;   collect   records a diagnostic in the sink and goes on, so that one compilation reports every
;;;             error it can find. `compile-tree-il` (the file compiler, which `load` of a script
;;;             goes through) runs the expander in this mode and raises one condition at its end,
;;;             see `raise-collected-errors`
;;;
;;; The sink is a box (a list of one element) of entries `(diagnostic . condition)`, newest
;;; first: the diagnostic (see (core diagnostics)) and the condition `raise` mode raises for the
;;; same error, or #f.

(define expansion-error-mode (make-parameter 'raise))
(define current-diagnostic-sink (make-parameter #f))

;; Calls THUNK with the mode MODE and the sink SINK (a new one is made by `make-diagnostic-sink`).
(define (call-with-expansion-errors mode sink thunk)
  (let ([saved-mode #f] [saved-sink #f])
    (dynamic-wind
      (lambda ()
        (set! saved-mode (expansion-error-mode))
        (set! saved-sink (current-diagnostic-sink))
        (expansion-error-mode mode)
        (current-diagnostic-sink sink))
      thunk
      (lambda ()
        (expansion-error-mode saved-mode)
        (current-diagnostic-sink saved-sink)))))

;;; The state of one expansion
;;;
;;; What the expander keeps beside the terms it builds is in a vector, the value of the parameter
;;; `current-expansion-state`: the fuel and the facts about the definition that is being expanded.
;;; An expansion that nobody opened a state for (`eval`, the compiler) gets one for its thread
;;; when it first needs it.

;; The macro uses one top-level form may have, and the macro uses that expand into a macro use,
;; one after the other: what keeps a macro that does not end from running for ever.
(define expansion-fuel-limit 100000)
(define macro-chain-limit 1000)

(define current-expansion-state (make-parameter #f))

;; #(fuel failed-definer)
(define (make-expansion-state)
  (vector expansion-fuel-limit #f))

(define (expansion-state)
  (or (current-expansion-state)
    (let ([state (make-expansion-state)])
      (current-expansion-state state)
      state)))

(define (expansion-state-fuel s) (vector-ref s 0))
(define (set-expansion-state-fuel! s v) (vector-set! s 0 v))
;; The definition form that did not expand, as `syntax-type` classified it: (kind id . mod), or #f
(define (expansion-state-failed-definer s) (vector-ref s 1))
(define (set-expansion-state-failed-definer! s v) (vector-set! s 1 v))

;; A sink is (entries count dropped): the entries newest first, how many there are, and how many
;; diagnostics were left out for the limit.
(define (make-diagnostic-sink) (list '() 0 0))

;; The most diagnostics a compilation keeps
(define diagnostic-limit 200)

;; Whether errors are being collected: the sink when they are, otherwise #f.
(define (collecting-sink)
  (and (memq (expansion-error-mode) '(collect)) (current-diagnostic-sink)))

;; The entries of the sink, (diagnostic . condition) in the order they were added, and when the
;; limit made it leave diagnostics out, one more that says how many.
(define (sink-entries sink)
  (let ([entries (reverse (car sink))] [dropped (car (cddr sink))])
    (if (> dropped 0)
      (append entries
        (list (cons (diagnostic-vector 'warning 'diagnostics-truncated
                      (string-append (number->string dropped)
                        (if (= dropped 1) " more diagnostic is not shown" " more diagnostics are not shown")
                        " (the limit is " (number->string diagnostic-limit) ")")
                      #f '() '() '())
                #f)))
      entries)))

;; Records the diagnostic D, and COND, the condition that raising it would raise, unless the sink
;; has it already (the same code, message and primary span). A full sink counts the ones it leaves
;; out.
(define (sink-add! sink d cond)
  (if (not (let loop ([entries (car sink)])
             (and (pair? entries)
               (let ([e (car (car entries))])
                 (or (and (eq? (vector-ref e 2) (vector-ref d 2))
                        (equal? (vector-ref e 3) (vector-ref d 3))
                        (equal? (vector-ref e 4) (vector-ref d 4)))
                   (loop (cdr entries)))))))
    (if (< (cadr sink) diagnostic-limit)
      (begin
        (set-car! sink (cons (cons d cond) (car sink)))
        (set-car! (cdr sink) (+ (cadr sink) 1)))
      (set-car! (cddr sink) (+ (car (cddr sink)) 1)))))

;; The diagnostic of a condition (or any raised object) comes from `condition->diagnostic` of
;; `(core diagnostics)`, which core code cannot import and is loaded when there is an error to
;; explain; `set-condition-diagnostician!` installs the procedure that calls it.
(define condition-diagnostician #f)
(define (set-condition-diagnostician! proc) (set! condition-diagnostician proc))

(define (diagnose-condition c)
  (let ([failure #f])
    (define (message-of x)
      (if (and (condition? x) (message-condition? x))
        (let ([m (condition-message x)]) (if (string? m) m "error"))
        "error"))
    (or (and condition-diagnostician
          (call/cc
            (lambda (k)
              (with-exception-handler
                (lambda (e) (set! failure e) (k #f))
                (lambda () (condition-diagnostician c))))))
      ;; the diagnostician failed, or there is none: the diagnostic has the message, and says that
      ;; the explanation of the error is missing when it was an error of the diagnostician
      (diagnostic-vector 'error 'error
        (if failure
          (string-append (message-of c) " (the explanation of this error failed: " (message-of failure) ")")
          (message-of c))
        #f '() '() '()))))

;; Records the diagnostic of the raised object C in SINK, with C if it is a condition (the
;; condition raise mode raises for the same error); returns the diagnostic.
(define (sink-add-condition! sink c)
  (let ([d (diagnose-condition c)])
    (sink-add! sink d (and (condition? c) c))
    d))

;; The span of a source vector, as (core diagnostics) has it: #(file line col end-line end-col
;; start-byte end-byte), or #f. A position inside the expansion of a macro is the use site.
(define (sourcev->diagnostic-span sv)
  (let loop ([sv sv])
    (cond
      [(and (vector? sv) (>= (vector-length sv) 9) (eq? (vector-ref sv 7) 'macro)
          (pair? (vector-ref sv 8)) (vector? (car (vector-ref sv 8))))
        (loop (car (vector-ref sv 8)))]
      [(and (vector? sv) (>= (vector-length sv) 3)
          (exact-integer? (vector-ref sv 1)) (exact-integer? (vector-ref sv 2)))
        (let* ([n (vector-length sv)]
               [slot (lambda (i) (and (< i n) (vector-ref sv i)))]
               [line (vector-ref sv 1)]
               [col (vector-ref sv 2)])
          (vector (vector-ref sv 0) line col
            (if (exact-integer? (slot 3)) (slot 3) line)
            (if (exact-integer? (slot 4)) (slot 4) (+ col 1))
            (and (exact-integer? (slot 5)) (slot 5))
            (and (exact-integer? (slot 6)) (slot 6))))]
      [else #f])))

(define (span<? a b)
  (cond
    [(not a) #f]
    [(not b) #t]
    [(not (equal? (vector-ref a 0) (vector-ref b 0)))
      (string<? (format "~a" (vector-ref a 0)) (format "~a" (vector-ref b 0)))]
    [(not (= (vector-ref a 1) (vector-ref b 1))) (< (vector-ref a 1) (vector-ref b 1))]
    [else (< (vector-ref a 2) (vector-ref b 2))]))

;; The entries ENTRIES (`(diagnostic . condition)`) in the order of their primary spans; the ones
;; without a span come last. Entries are merged into a sorted list one by one, so the order
;; of entries with the same span is kept.
(define (sort-diagnostic-entries entries)
  (define (insert e sorted)
    (cond
      [(null? sorted) (list e)]
      [(span<? (vector-ref (car (car sorted)) 4) (vector-ref (car e) 4))
        (cons (car sorted) (insert e (cdr sorted)))]
      [else (cons e sorted)]))
  (let loop ([entries entries] [out '()])
    (if (null? entries)
      out
      (loop (cdr entries) (insert (car entries) out)))))

(define (diagnostic-vector severity code message primary labels notes helps)
  (vector '|#%diagnostic| severity code message primary labels notes helps '() '()))

;; Suggestions for an unbound name come from `(core suggest)`, which core code cannot import
;; and is loaded when there is an error to explain. The procedure installed with
;; `set-unbound-suggester!` is called with the symbol and the symbols visible in the unit, and
;; returns a list of help strings.
(define unbound-suggester #f)
(define (set-unbound-suggester! proc) (set! unbound-suggester proc))

;; The symbols the environment ENV binds, its own and the ones of the libraries it imports.
(define (env-visible-symbols env)
  (let ([seen (make-core-hash-eq)] [out '()])
    (for-each
      (lambda (e)
        (env-for-each
          (lambda (sym var)
            (if (not (core-hash-ref seen sym #f))
              (begin
                (core-hash-put! seen sym #t)
                (set! out (cons sym out)))))
          e))
      (env-and-imports env))
    out))

;; The condition that raising the error `(error WHO MSG irritant ...)` would raise.
(define (error-condition who msg . irritants)
  (call/cc
    (lambda (k)
      (with-exception-handler
        (lambda (c) (k c))
        (lambda () (apply error who msg irritants))))))

;; The condition raise mode raises for the first name of UNDEFINED, the names U refers to and does
;; not define.
(define (unbound-condition u sym)
  (if (program-key? (unit-key u))
    (error-condition 'compile (string-append "unbound identifier: " (symbol->string sym)))
    (error-condition 'compile "unbound variable" sym (unit-key u))))

;; The diagnostics for the names of UNDEFINED, one for each place that refers to a name, in the
;; order of the first reference, as entries of the sink.
(define (unbound-entries u undefined)
  (let ([names #f]
        [refs (reverse (%unit-forward-refs u))])
    (define (helps sym)
      (if unbound-suggester
        (let ([visible (or names (begin (set! names (env-visible-symbols (unit-env u))) names))])
          (call/cc
            (lambda (k)
              (with-exception-handler
                (lambda (c) (k '()))
                (lambda () (unbound-suggester sym visible))))))
        '()))
    ;; A unit that imports a library that does not exist cannot tell which of the names it
    ;; does not bind come from that library: they are hints, and say so.
    (define unresolved
      (let loop ([in (reverse (unit-unresolved u))] [out '()])
        (cond
          [(null? in) (reverse out)]
          [(member (cdr (car in)) out) (loop (cdr in) out)]
          [else (loop (cdr in) (cons (cdr (car in)) out))])))
    (define (entry sym src)
      (let ([span (and src (sourcev->diagnostic-span src))])
        (cons
          (diagnostic-vector (if (null? unresolved) 'error 'hint) 'unbound-variable
            (string-append "unbound variable `" (symbol->string sym) "`")
            span
            (if span (list (list span 'primary "not bound in this scope")) '())
            (if (null? unresolved)
              '()
              (list (string-append "it may come from the unresolved library "
                      (let join ([names unresolved])
                        (if (null? (cdr names))
                          (car names)
                          (string-append (car names) ", " (join (cdr names))))))))
            (helps sym))
          #f)))
    (let loop ([syms undefined] [out '()])
      (if (null? syms)
        (reverse out)
        (let* ([sym (car syms)]
               [sites (let site-loop ([refs refs] [sites '()])
                        (cond
                          [(null? refs) (reverse sites)]
                          [(eq? (car (car refs)) sym) (site-loop (cdr refs) (cons (cdr (car refs)) sites))]
                          [else (site-loop (cdr refs) sites)]))]
               [entries (map-in-order (lambda (src) (entry sym src)) (if (null? sites) '(#f) sites))])
          (loop (cdr syms) (append (reverse entries) out)))))))

;; BODY with the references and assignments to the variables of UNDEFINED, the names U does not
;; define, replaced by error terms.
(define (replace-unbound-variables u body undefined)
  (let ([key (unit-key u)] [own (%unit-own u)])
    (define (unbound? sym slot)
      (and (memq sym undefined) (eq? slot (core-hash-ref own sym #f))))
    (let rewrite ([t body])
      (cond
        [(and (global-ref? t) (equal? (global-ref-key t) key)
            (unbound? (global-ref-name t) (global-ref-slot t)))
          (make-unbound-variable-term (term-src t) (global-ref-name t) key #f)]
        [(and (global-set? t) (equal? (global-set-key t) key)
            (unbound? (global-set-name t) (global-set-slot t)))
          (make-unbound-variable-term (term-src t) (global-set-name t) key
            (rewrite (global-set-value t)))]
        [else (term-map-children t rewrite)]))))

;; Whether the term T has an error term.
(define (term-contains-errors? t)
  (let walk ([t t])
    (cond
      [(or (unbound-variable-term? t) (syntax-error-term? t)) #t]
      [(or (constant? t) (void? t) (lref? t) (primref? t) (global-ref? t)) #f]
      [(lset? t) (walk (lset-value t))]
      [(global-set? t) (walk (global-set-value t))]
      [(if? t) (or (walk (if-test t)) (walk (if-then t)) (walk (if-else t)))]
      [(let? t) (or (any-term? walk (let-rhs t)) (walk (let-body t)))]
      [(fix? t) (or (any-term? walk (fix-rhs t)) (walk (fix-body t)))]
      [(receive? t) (or (walk (receive-producer t)) (walk (receive-consumer t)))]
      [(application? t) (or (walk (application-operator t)) (any-term? walk (application-operands t)))]
      [(primcall? t) (any-term? walk (primcall-args t))]
      [(proc? t) (walk (proc-body t))]
      [(linklet-term? t) (walk (linklet-term-body t))]
      [(values? t) (any-term? walk (values-values t))]
      [(sequence? t) (or (walk (sequence-head t)) (walk (sequence-tail t)))]
      [(wcm? t) (or (walk (wcm-key t)) (walk (wcm-mark t)) (walk (wcm-result t)))]
      [else #f])))

(define (any-term? walk ts)
  (and (pair? ts) (or (walk (car ts)) (any-term? walk (cdr ts)))))

(define (il-terms il)
  (if (and (application? il) (primref? (application-operator il)))
    (application-operands il)
    (list il)))

;; Called when the compilation of a file is done, with the terms IL: raises the errors the sink
;; collected. The condition has the simple conditions of the first error, the ones it would have
;; raised on its own, so that handlers for them work as they do in `raise` mode, and a
;; `&compilation-errors` with the diagnostics of all of them, warnings and hints included.
;; Warnings and hints alone do not fail the compilation: see `sink-warnings`. IL is checked
;; for error terms that have no diagnostic.
(define (raise-collected-errors sink il)
  (let* ([entries (sink-entries sink)]
         [errors (filter (lambda (e) (eq? (vector-ref (car e) 1) 'error)) entries)])
    (cond
      [(pair? errors)
        (raise
          (condition
            (or (cdr (car errors)) (make-error))
            (make-compilation-errors-condition (map-in-order car entries))))]
      [(let loop ([ts (il-terms il)])
         (and (pair? ts) (or (term-contains-errors? (car ts)) (loop (cdr ts)))))
        (error 'compile "internal error: the compiled code has an error term without a diagnostic")])))

;; The diagnostics of the sink that are not errors (warnings, hints), in the order of their
;; spans. They are what is left to report when `raise-collected-errors` did not raise.
(define (sink-warnings sink)
  (map-in-order car
    (sort-diagnostic-entries
      (filter (lambda (e) (not (eq? (vector-ref (car e) 1) 'error))) (sink-entries sink)))))

;;; Lints
;;;
;;; Checks for code that compiles and is probably wrong, reported as warnings. They run when
;;; `lint-calls?` is on (`capy-rt --compile --lint`, which `capy check` passes).
;;;
;;; A call of a procedure the compiler knows the arity of (see "Knowns": one that an imported
;;; library defines once and never assigns, or the unit defines itself that way) with a number of
;;; arguments it does not take is a `call/arity` warning on the call. The call still raises when
;;; it runs, so this is a warning and not an error: the code may be unreachable.

(define lint-calls? (make-parameter #f))

;; What a procedure with the arity mask MASK takes, for a message: "2 arguments", "at least 1
;; argument", "1 or 3 arguments".
(define (arity-mask-text mask)
  (define (arguments n) (string-append (number->string n) (if (= n 1) " argument" " arguments")))
  (if (< mask 0)
    (let loop ([n 0] [bit 1])
      (if (>= (+ mask bit) 0)
        (string-append "at least " (arguments n))
        (loop (+ n 1) (* bit 2))))
    (let loop ([n 0] [m mask] [counts '()])
      (cond
        [(= m 0)
          (let ([counts (reverse counts)])
            (cond
              [(null? (cdr counts)) (arguments (car counts))]
              [else
                (let join ([cs counts] [out ""])
                  (cond
                    [(null? cs) (string-append out " arguments")]
                    [else
                      (join (cdr cs)
                        (string-append out
                          (cond [(string=? out "") ""] [(null? (cdr cs)) " or "] [else ", "])
                          (number->string (car cs))))]))]))]
        [else (loop (+ n 1) (quotient m 2) (if (odd? m) (cons n counts) counts))]))))

;; The name of the library whose key is KEY, as the program that imports it writes it: the keys of
;; the libraries of a package start with `#%pkg` and the package.
(define (library-key-name key)
  (if (and (pair? key) (eq? (car key) '|#%pkg|) (pair? (cdr key)) (pair? (cddr key)))
    (cddr key)
    key))

;; Adds a `call/arity` warning to SINK for every call in BODY, the expansion of the forms of U,
;; of a procedure with a known arity that has another number of arguments. DEFS are the
;; `global-set` terms of the definitions of U.
(define (lint-call-arities! u body defs sink)
  (let ([tables (imported-knowns-tables u)]
        [key (unit-key u)]
        [counts (make-core-hash-eq)]
        [procs (make-core-hash-eq)])
    ;; (arity-mask . defined-at) of the operator OP, or #f
    (define (known-arity op)
      (and (global-ref? op)
        (if (equal? (global-ref-key op) key)
          (let ([sym (global-ref-name op)])
            (and (= (core-hash-ref counts sym 0) 1)
              (not (memq sym (%unit-assigned u)))
              (let ([value (core-hash-ref procs sym #f)])
                (and value (proc? value)
                  (cons (formals-arity-mask (proc-args value))
                    (term-src value))))))
          (let ([known (or (known-in-tables tables op) (runtime-known op))])
            (and known (eq? (car known) 'procedure)
              (cons (cadr known) #f))))))
    ;; a procedure of the runtime itself (`car`, `vector-ref`: the libraries `(#%capy)` and the boot
    ;; ones) has no decl to say what it takes: the procedure does.
    (define (runtime-known op)
      (let ([key (global-ref-key op)])
        (and (pair? key) (memq (car key) '(boot |#%capy|))
          (let* ([inst (library-instance (current-library-registry) key)]
                 [var (and inst (instance-variable inst (global-ref-name op)))]
                 [value (and var (variable-bound? var) (variable-ref var))]
                 [mask (and (procedure? value) (procedure-arity-mask value))])
            (and mask (list 'procedure mask))))))
    (define (visit t)
      (when (and (application? t) (list? (application-operands t)))
        (let* ([op (application-operator t)]
               [arity (known-arity op)]
               [n (length (application-operands t))]
               [span (and arity (term-src t) (sourcev->diagnostic-span (term-src t)))])
          (when (and arity span (not (arity-mask-accepts? (car arity) n)))
            (let* ([name (symbol->string (global-ref-name op))]
                   [defined (and (cdr arity) (sourcev->diagnostic-span (cdr arity)))])
              (sink-add! sink
                (diagnostic-vector 'warning 'call/arity
                  (string-append "wrong number of arguments in the call of `" name "`")
                  span
                  (append
                    (list (list span 'primary
                            (string-append "`" name "` takes " (arity-mask-text (car arity))
                              ", but this call has " (number->string n))))
                    (if defined
                      (list (list defined 'secondary (string-append "`" name "` is defined here")))
                      '()))
                  (if defined
                    '()
                    (list (string-append "`" name "` is defined in "
                           (format "~a" (library-key-name (global-ref-key op))))))
                  '())
                #f)))))
      (term-map-children t (lambda (child) (visit child) child)))
    (for-each
      (lambda (t)
        (let ([sym (global-set-name t)])
          (core-hash-put! counts sym (+ 1 (core-hash-ref counts sym 0)))
          (core-hash-put! procs sym (global-set-value t))))
      defs)
    (visit body)))

(define (close-linking-unit u body0)
  (let* ([key (unit-key u)]
         [library? (eq? (unit-kind u) 'library)]
         [boot? (eq? (unit-kind u) 'boot)]
         [body (if (or library? boot?) (fold-imported-knowns u body0) body0)]
         [defs (own-definitions u (top-level-forms body))]
         [defined (dedupe-symbols (map-in-order global-set-name defs))]
         [defined-table (make-core-hash-eq)])
    (for-each (lambda (sym) (core-hash-put! defined-table sym #t)) defined)
    (let* ([undefined (filter (lambda (sym) (not (core-hash-ref defined-table sym #f)))
                        (reverse (%unit-own-order u)))]
           [sink (collecting-sink)])
      (if (and sink (lint-calls?))
        (lint-call-arities! u body defs sink))
      (if (pair? undefined)
        (if sink
          (let ([entries (sort-diagnostic-entries (unbound-entries u undefined))])
            ; the first error of the unit is the one raise mode raises, with its condition
            (set-cdr! (car entries) (unbound-condition u (car undefined)))
            (for-each (lambda (e) (sink-add! sink (car e) (cdr e))) entries)
            (set! body (replace-unbound-variables u body undefined))
            (%unit-set-own-order! u
              (filter (lambda (sym) (not (memq sym undefined))) (%unit-own-order u))))
          (if (program-key? key)
            (error 'compile (string-append "unbound identifier: " (symbol->string (car undefined))))
            (error 'compile "unbound variable" (car undefined) key)))))
    (if (pair? (unit-provide u))
      (for-each
        (lambda (sym)
          (if (not (core-hash-ref defined-table sym #f))
            (let ([sink (collecting-sink)])
              (if sink
                (sink-add! sink
                  (diagnostic-vector 'error 'export/not-defined
                    (string-append "`" (symbol->string sym) "` is provided but not defined")
                    #f '() '() '())
                  (error-condition '|#%provide| "not defined" sym key))
                (error '|#%provide| "not defined" sym key)))))
        (unit-provide u)))
    (let* ([exports (unit-exported-names u defined)]
           [private (let ([exported (symbol-set exports)])
                      (let loop ([syms defined] [out '()])
                        (cond
                          [(null? syms) (reverse out)]
                          [(core-hash-ref exported (car syms) #f) (loop (cdr syms) out)]
                          [else (loop (cdr syms) (cons (car syms) out))])))]
           [locals (localizable-definitions u defs
                     (cond
                       [(not (env-defines-macros? (unit-env u))) '()]
                       [library? (env-export-internals (unit-env u))]
                       [else exports]))]
           [body (bind-private-variables u (bind-local-definitions body locals) private)])
      (make-linklet-term #f key
        (unit-import-groups u)
        exports
        (append (unit-import-slots u)
          (map-in-order (lambda (sym) (core-hash-ref (%unit-own u) sym #f)) exports))
        body
        (if library?
          (env-library-decl (unit-env u)
            (if (program-key? key) '() (unit-knowns u defs))
            (map-in-order car (unit-import-groups u))
            defined)
          (list '|#%boot| (env-key (unit-env u))
            (if boot?
              (boot-unit-knowns u (own-definitions u (top-level-forms body0)) locals exports)
              '())))))))

;;; Files

(define (call-with-unit-sink sink thunk)
  (let ([saved #f])
    (dynamic-wind
      (lambda ()
        (set! saved (current-unit-sink))
        (current-unit-sink sink))
      thunk
      (lambda ()
        (current-unit-sink saved)))))

(define (unit-sink-push! t)
  (let ([sink (current-unit-sink)])
    (if (not sink)
      (error 'library "a library form needs a file to compile it into"))
    (set-car! sink (cons t (car sink)))))

(define (join-terms terms)
  (cond
    [(null? terms) (make-void #f)]
    [(null? (cdr terms)) (car terms)]
    [else (make-sequence #f (car terms) (join-terms (cdr terms)))]))

(define (unit-predeclare! u forms strip)
  (define (id x) (and (symbol? x) x))
  (define (declare! sym)
    (if sym (env-own-variable! (unit-env u) sym (unit-key u))))
  (define (formals! f)
    (cond
      [(pair? f) (declare! (id (car f))) (formals! (cdr f))]
      [else (declare! (id f))]))
  (define (head x)
    (and (pair? x) (id (car x))))
  (define (target! t)
    (cond
      [(pair? t) (target! (car t))]
      [else (declare! (id t))]))
  (define (scan form)
    (let ([h (head form)])
      (cond
        [(not h) #f]
        [(and (eq? h 'define) (pair? (cdr form)))
          (target! (cadr form))]
        [(and (eq? h 'define-values) (pair? (cdr form)))
          (formals! (cadr form))]
        [(eq? h 'begin)
          (let loop ([rest (cdr form)])
            (if (pair? rest)
              (begin (scan (car rest)) (loop (cdr rest)))))])))
  (if (unit-namespace? u)
    (let loop ([forms forms])
      (if (pair? forms)
        (let ([form (strip (car forms))])
          (if (not (memq (head form) '(library define-library)))
            (begin
              (scan form)
              (loop (cdr forms)))))))))

(define (unit-add-body! t)
  (let ([u (current-unit)])
    (%unit-set-body! u (cons t (%unit-body u)))))

(define (finish-current-unit!)
  (let* ([u (current-unit)]
         [body (join-terms (reverse (%unit-body u)))])
    (%unit-set-body! u '())
    (if (unit-has-content? u body)
      (unit-sink-push! (unit-close! u body)))))

(define (expand-file-units forms env key expand-form)
  (let ([sink (list '())]
        [unit (make-file-unit env key 'c)])
    (call-with-unit-sink sink
      (lambda ()
        (call-with-unit unit
          (lambda ()
            (for-each
              (lambda (form) (unit-add-body! (expand-form form)))
              forms)
            (finish-current-unit!)))))
    (reverse (car sink))))

($set-unit-variable-hook! unit-variable)

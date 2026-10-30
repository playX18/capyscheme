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
;;;   slots    the slot variable (lvar) of every variable the body refers to, by the
;;;            linklet that owns the variable: an import slot of the group of its key,
;;;            or an export slot when the unit itself owns it
;;;   late     the names of `(late key name ...)` imports, see `unit-require!`
;;;   assigned the own names that are assigned (`set!`)
;;   body     the expansions of the forms of the unit so a file driver has seen, newest first
;;;
;;; The expander asks the unit for a slot when it builds a global reference, assignment or
;;; definition (`unit-binding-slot`, `unit-definition-slot!`), from the `(key . sym)` the
;;; environment binding says its variable is owned by (see `env-origin`). The terms it
;;; builds are `global-ref` and `global-set` on those slots. Nothing is resolved after the
;;; fact, except by `unit-close!`, which can use what the whole body shows:
;;;
;;; - A unit of kind `library` or `boot` exports every definition of its body, whatever
;;;   the export table says: macros of the library can expand into references to its
;;;   private definitions in other units, and those references are made on the variables
;;;   of the instance. What importers see is the export table of the meta of the linklet.
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

(define (unit-kind u) (vector-ref u 1))
(define (unit-key u) (vector-ref u 2))
(define (unit-env u) (vector-ref u 3))
(define (%unit-groups u) (vector-ref u 4))
(define (%unit-own u) (vector-ref u 5))
(define (%unit-own-order u) (vector-ref u 6))
(define (%unit-assigned u) (vector-ref u 7))
(define (%unit-late u) (vector-ref u 8))
(define (%unit-counter u) (vector-ref u 9))
(define (unit-parent u) (vector-ref u 10))
(define (unit-provide u) (or (vector-ref u 11) '()))
(define (%unit-body u) (vector-ref u 12))

(define (unit? x)
  (and (vector? x) (> (vector-length x) 12) (eq? (vector-ref x 0) '%unit)))

;; Whether U runs in a namespace.
(define (unit-namespace? u) (namespace? (unit-env u)))

;; A new unit. Scratch units take a PARENT, whose counter they use.
(define (make-unit kind key env . parent)
  (let ([u (vector '%unit kind key env '() (make-core-hash-eq) '() '() '() 0
             (if (pair? parent) (car parent) #f) #f '())])
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
  (vector-set! u 11
    (append (unit-provide u)
      (let loop ([specs specs])
        (cond
          [(null? specs) '()]
          [(symbol? (car specs))
            (if (eq? (car specs) 'all-defined)
              (loop (cdr specs))
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
        (vector-set! u 9 (+ n 1))
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
          (vector-set! u 4 (cons group (%unit-groups u)))
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
        (vector-set! u 6 (cons sym (%unit-own-order u)))
        slot))))

;; The key of the late import group that lists SYM, or #f.
(define (unit-late-key u sym)
  (let ([entry (assq sym (%unit-late u))])
    (and entry (cdr entry))))

;; The slot a term needs to refer to SYM as seen from KEY (the key of the environment
;; the identifier is bound in) or from its public
;; interface when PUBLIC? is true. The result is a list
;;
;;   (key sym slot)
;;
;; where `(key . sym)` names the variable in the linklet that owns it. The owner is in
;; the environment binding: its origin. A name that is late-bound by a header of U is the
;; variable of the late key even if the environment does not bind it yet. A name that
;; nothing binds is a forward reference to a definition of U (it is checked when U is
;; closed), or a variable of the namespace of U.
(define (unit-binding-slot u key sym public?)
  (let* ([env (unit-env u)]
         [base (if (or (not key) (equal? key (env-key env))) env (global-environment key))]
         [scope (if public? (env-public-interface base) base)]
         [origin (or (env-origin scope sym)
                   (and key (not (equal? key (env-key env))) (cons key sym)))]
         [own? (and origin (not (unit-namespace? u)) (equal? (car origin) (unit-key u)))]
         [late (and (not own?) (not public?) (eq? base env) (unit-late-key u sym))])
    (cond
      [late (list late sym (unit-import-slot! u late sym))]
      [own? (list (car origin) (cdr origin) (unit-own-slot! u (cdr origin)))]
      [origin (list (car origin) (cdr origin) (unit-import-slot! u (car origin) (cdr origin)))]
      [(unit-namespace? u) (list (unit-key u) sym (unit-import-slot! u (unit-key u) sym))]
      [else (list (unit-key u) sym (unit-own-slot! u sym))])))

(define (unit-assignment-slot u key sym public?)
  (let ([found (unit-binding-slot u key sym public?)])
    (if (and (eq? (caddr found) (core-hash-ref (%unit-own u) (cadr found) #f))
         (not (memq (cadr found) (%unit-assigned u))))
      (vector-set! u 7 (cons (cadr found) (%unit-assigned u))))
    found))

;; The variable SYM that U defines
(define (unit-definition-slot! u sym)
  (env-own-variable! (unit-env u) sym (unit-key u))
  (if (unit-namespace? u)
    (list (unit-key u) sym (unit-import-slot! u (unit-key u) sym))
    (list (unit-key u) sym (unit-own-slot! u sym))))

;;; Headers

;; Applies a `|#%require|` of a boot unit
(define (unit-require! u specs)
  (for-each
    (lambda (spec)
      (if (and (pair? spec) (eq? (car spec) 'late))
        (for-each
          (lambda (sym)
            (vector-set! u 8 (cons (cons sym (cadr spec)) (%unit-late u))))
          (cddr spec))))
    specs))

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
    [else (error 'term-map-children "unknown TreeIL term" t)]))

(define (map-in-order f ls)
  (if (null? ls)
    '()
    (let* ([x (f (car ls))]
           [rest (map-in-order f (cdr ls))])
      (cons x rest))))

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

(define (localizable-definitions u defs)
  (let ([counts (make-core-hash-eq)]
        [exported (env-export-internals (unit-env u))])
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
                   (not (memq sym exported))
                   (not (memq sym (%unit-assigned u))))
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
      (make-linklet-term #f (unit-key u)
        (unit-import-groups u) '() (unit-import-slots u) body
        (namespace-meta (unit-env u)))]
    [else (error 'unit-close! "cannot close the unit" (unit-kind u))]))

(define (program-key? key)
  (and (pair? key) (eq? (car key) '%program)))

(define (close-linking-unit u body)
  (let* ([key (unit-key u)]
         [library? (eq? (unit-kind u) 'library)]
         [defs (own-definitions u (top-level-forms body))]
         [defined (map-in-order global-set-name defs)]
         [defined-table (make-core-hash-eq)])
    (for-each (lambda (sym) (core-hash-put! defined-table sym #t)) defined)
    (for-each
      (lambda (sym)
        (if (not (core-hash-ref defined-table sym #f))
          (if (program-key? key)
            (error 'compile (string-append "unbound identifier: " (symbol->string sym)))
            (error 'compile "unbound variable" sym key))))
      (reverse (%unit-own-order u)))
    (if (pair? (unit-provide u))
      (for-each
        (lambda (sym)
          (if (not (core-hash-ref defined-table sym #f))
            (error '|#%provide| "not defined" sym key)))
        (unit-provide u)))
    (let* ([exports (let loop ([syms defined] [seen '()])
                      (cond
                        [(null? syms) (reverse seen)]
                        [(memq (car syms) seen) (loop (cdr syms) seen)]
                        [else (loop (cdr syms) (cons (car syms) seen))]))]
           [locals (if library? (localizable-definitions u defs) '())]
           [body (bind-local-definitions body locals)])
      (make-linklet-term #f key
        (unit-import-groups u)
        exports
        (append (unit-import-slots u)
          (map-in-order (lambda (sym) (core-hash-ref (%unit-own u) sym #f)) exports))
        body
        (if library?
          (library-meta (unit-env u))
          (cons '|#%boot|
            (cons (env-key (unit-env u))
              (let loop ([late (%unit-late u)] [keys '()])
                (cond
                  [(null? late) (reverse keys)]
                  [(let has ([ks keys]) (and (pair? ks) (or (equal? (car ks) (cdr (car late))) (has (cdr ks))))) (loop (cdr late) keys)]
                  [else (loop (cdr late) (cons (cdr (car late)) keys))])))))))))

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
    (vector-set! u 12 (cons t (%unit-body u)))))

(define (finish-current-unit!)
  (let* ([u (current-unit)]
         [body (join-terms (reverse (%unit-body u)))])
    (vector-set! u 12 '())
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

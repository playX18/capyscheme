;;; -*- mode: scheme; coding: utf-8; -*-
;;; Expansion environments: the compile-time bindings of a library, an
;;; interface, or a namespace.
;;;
;;; An environment is a record (so the table of its fields is the one place to read), with these
;;; fields:
;;;   key          library name (list of symbols) or #f
;;;   kind         'library | 'interface | 'namespace
;;;   locals       eq table: symbol -> variable
;;;   imports      list of interface environments, searched in order
;;;   cache        eq table: symbol -> binding found through imports
;;;   public       the public interface environment, or #f until created
;;;   replacements eq table: symbol -> #t
;;;   unique-id    counter for gensym and unique ids
;;;   instance     for namespaces, the mutable instance whose variables are `locals` (see
;;;                namespace.scm), #f otherwise
;;;   frozen       for namespaces, #t once `namespace-freeze!` has been called
;;;   pending      import entries not applied yet (see `env-add-pending-imports!`)
;;;   applied      import entries already applied to the environment
;;;   origins      eq table: symbol -> (key . sym), the binding of the symbol in `locals`
;;;                as its owner names it: the key of the linklet that defines the variable
;;;                and the name it has there. The bindings that come through imports have
;;;                theirs in the imports (see `env-origin`).
;;;   registry     for namespaces, the library registry their libraries live in (see
;;;                `current-library-registry`), #f otherwise
;;;
;;; and the facts of the library declaration being compiled in the environment (see "Library
;;; decls" below), which `env-begin-unit!` resets:
;;;   version      the version reference of the library form, a list
;;;   language     the execution mode the library was declared in
;;;   export-log   the exports noted so far, newest first: (public internal replace? kind)
;;;   import-log   the import entries recorded so far, newest first: (mode . import-set)
;;;   macros       eq table: internal symbol -> #t, the names the library defines as syntax
;;;   deps         the keys of the libraries imported while the library was expanded, newest first
;;;   decl         the decl of the library (see "Library decls" below), once it is compiled or
;;;                loaded; #f before
;;;
;;; and, for an interface that is a trimmed copy (see `env-use-interfaces!`):
;;;   source       the interface it was copied from, #f for any other environment
;;;
;;; Each library registry maps library keys to environments.

(|#%require| (boot prim) (boot control) (boot records) (boot raise) (boot linker))
(|#%provide| all-defined)

;;; Hooks
;;;
;;; The library manager (library-manager.scm) and the expander (psyntax-exp.scm) are built on
;;; the environments, so they install what the environments need from them when they are
;;; instantiated: the execution mode, the loader of a library file, the message for a library
;;; that cannot be found and the resolver of R6RS interfaces.

(define $execution-mode-hook #f)
(define $load-library-hook #f)
(define $library-not-found-message-hook #f)
(define ($set-library-manager-hooks! execution-mode load-library not-found-message)
  (set! $execution-mode-hook execution-mode)
  (set! $load-library-hook load-library)
  (set! $library-not-found-message-hook not-found-message))
(define ($execution-mode . args) (apply $execution-mode-hook args))

(define $resolve-r6rs-interface-hook #f)
(define ($set-resolve-r6rs-interface-hook! proc) (set! $resolve-r6rs-interface-hook proc))

;;; The record

(define &env
  (let* ([rtd (make-record-type-descriptor '&env #f #f #f #f
                '#((immutable key) (immutable kind) (immutable locals) (mutable imports)
                   (immutable cache) (mutable public) (immutable replacements)
                   (mutable unique-id) (mutable instance) (mutable frozen)
                   (mutable pending) (mutable applied) (immutable origins) (mutable registry)
                   (mutable version) (mutable language) (mutable export-log)
                   (mutable import-log) (mutable macros) (mutable deps) (mutable decl)
                   (mutable source)))]
         [rcd (make-record-constructor-descriptor rtd #f #f)])
    (make-record-type '&env rtd rcd)))

(define %make-env (record-constructor (record-type-rcd &env)))
(define env? (record-predicate (record-type-rtd &env)))

(define %env-key (record-accessor (record-type-rtd &env) 0))
(define %env-kind (record-accessor (record-type-rtd &env) 1))
(define %env-locals (record-accessor (record-type-rtd &env) 2))
(define %env-raw-imports (record-accessor (record-type-rtd &env) 3))
(define %env-set-imports! (record-mutator (record-type-rtd &env) 3))
(define %env-cache (record-accessor (record-type-rtd &env) 4))
(define %env-public (record-accessor (record-type-rtd &env) 5))
(define %env-set-public! (record-mutator (record-type-rtd &env) 5))
(define %env-replacements (record-accessor (record-type-rtd &env) 6))
(define %env-unique-id (record-accessor (record-type-rtd &env) 7))
(define %env-set-unique-id! (record-mutator (record-type-rtd &env) 7))
(define env-instance (record-accessor (record-type-rtd &env) 8))
(define env-set-instance! (record-mutator (record-type-rtd &env) 8))
(define env-frozen? (record-accessor (record-type-rtd &env) 9))
(define env-set-frozen! (record-mutator (record-type-rtd &env) 9))
(define %env-pending (record-accessor (record-type-rtd &env) 10))
(define %env-set-pending! (record-mutator (record-type-rtd &env) 10))
(define %env-applied (record-accessor (record-type-rtd &env) 11))
(define %env-set-applied! (record-mutator (record-type-rtd &env) 11))
(define %env-origins (record-accessor (record-type-rtd &env) 12))
(define env-library-registry (record-accessor (record-type-rtd &env) 13))
(define env-set-library-registry! (record-mutator (record-type-rtd &env) 13))
(define %env-version (record-accessor (record-type-rtd &env) 14))
(define %env-set-version! (record-mutator (record-type-rtd &env) 14))
(define %env-language (record-accessor (record-type-rtd &env) 15))
(define %env-set-language! (record-mutator (record-type-rtd &env) 15))
(define %env-export-log (record-accessor (record-type-rtd &env) 16))
(define %env-set-export-log! (record-mutator (record-type-rtd &env) 16))
(define %env-import-log (record-accessor (record-type-rtd &env) 17))
(define %env-set-import-log! (record-mutator (record-type-rtd &env) 17))
(define %env-macros (record-accessor (record-type-rtd &env) 18))
(define %env-deps (record-accessor (record-type-rtd &env) 19))
(define %env-set-deps! (record-mutator (record-type-rtd &env) 19))
(define %env-decl (record-accessor (record-type-rtd &env) 20))
(define %env-set-decl! (record-mutator (record-type-rtd &env) 20))
(define %env-source (record-accessor (record-type-rtd &env) 21))
(define %env-set-source! (record-mutator (record-type-rtd &env) 21))

;; The imports of ENV. Imports a loaded script left for ENV (see
;; `env-add-pending-imports!`) are applied the first time they are asked for.
(define (%env-imports env)
  (if (pair? (%env-pending env))
    (env-force-pending-imports! env))
  (%env-raw-imports env))

(define env-kinds '(library interface namespace))

;; Whether X is a namespace: an environment of kind 'namespace (namespace.scm).
(define (namespace? x)
  (and (env? x) (eq? (%env-kind x) 'namespace)))

;; A new environment of KEY and KIND (see the field list above). Interface
;; environments keep their bindings in `locals`, so `env-add!` on an interface fills its
;; exports.
(define (make-env key kind)
  (if (not (memq kind env-kinds))
    (assertion-violation 'make-env "invalid environment kind" kind))
  (%make-env key kind (make-core-hash-eq) '() (make-core-hash-eq) #f (make-core-hash-eq) 0
    #f #f '() '() (make-core-hash-eq) #f
    '() 'compat '() '() (make-core-hash-eq) '() #f #f))

;; A copy of ENV with tables of its own (the bindings, the cache of imports, the
;; replacements and the origins). The imports and the public interface are shared.
(define (env-clone env)
  (let ([new (%make-env (%env-key env) (%env-kind env)
               (core-hash-copy (%env-locals env)) (%env-raw-imports env)
               (core-hash-copy (%env-cache env)) (%env-public env)
               (core-hash-copy (%env-replacements env)) (%env-unique-id env)
               (env-instance env) (env-frozen? env) (%env-pending env) (%env-applied env)
               (core-hash-copy (%env-origins env)) (env-library-registry env)
               (%env-version env) (%env-language env) (%env-export-log env)
               (%env-import-log env) (core-hash-copy (%env-macros env))
               (%env-deps env) (%env-decl env) (%env-source env))])
    new))

(define (env-key env) (%env-key env))
(define (env-kind env) (%env-kind env))
;; The environment expansion runs in. Entry points (`compile-tree-il`, `eval`)
;; set it for the extent of the expansion. When it is not set, the core
;; environment is used (boot files expand in it).
(define current-expansion-env (make-parameter #f))

;; The namespace of the REPL, `eval`, and `load` of non-library scripts. See
;; namespace.scm.
(define current-namespace (make-parameter #f))

;; The registry libraries are looked up and instantiated in: the one of the current
;; namespace. Before there is a namespace (early boot) and for a namespace without a
;; registry of its own, it is the root registry.
(define (current-library-registry)
  (let ([ns (current-namespace)])
    (or (and ns (env-library-registry ns))
      (root-library-registry))))

(define (expansion-env)
  (or (current-expansion-env) the-core-env))

;; Calls THUNK with the expansion environment set to ENV, restoring the previous one
;; when THUNK returns or escapes.
(define (call-with-expansion-env env thunk)
  (let ([saved #f])
    (dynamic-wind
      (lambda ()
        (set! saved (current-expansion-env))
        (current-expansion-env env))
      thunk
      (lambda ()
        (current-expansion-env saved)))))

;; Calls THUNK with the current namespace set to NS, restoring the previous one when THUNK
;; returns or escapes. Libraries are looked up in the registry of the current namespace, so
;; this runs THUNK in the registry of NS.
(define (call-with-current-namespace ns thunk)
  (let ([saved #f])
    (dynamic-wind
      (lambda ()
        (set! saved (current-namespace))
        (current-namespace ns))
      thunk
      (lambda ()
        (current-namespace saved)))))

;;; Lookup

;; The variable bound to SYM directly in ENV, or #f.
(define (env-local-variable env sym)
  (core-hash-ref (%env-locals env) sym #f))

;; Like `env-local-variable`, but creates an unbound variable when absent. The new
;; variable is the one ENV's owner defines under SYM.
(define (env-ensure-local-variable! env sym)
  (or (env-local-variable env sym)
    (let ([var (make-variable)])
      (core-hash-put! (%env-locals env) sym var)
      (core-hash-remove! (%env-cache env) sym)
      var)))

;; The variable of the definition of SYM by the unit KEY in ENV. The variable of ENV
;; is kept when the unit defines it already (a redefinition in a namespace is the same
;; variable). Otherwise the unit makes a variable of its own, in place of what ENV
;; had: a binding that an earlier unit defined (a boot file redefining a name) or a
;; forward reference.
(define (env-own-variable! env sym key)
  (let ([var (env-local-variable env sym)])
    (if (and var
         (let ([origin (%env-origin-of env sym)])
           (and (eq? (cdr origin) sym) (equal? (car origin) key))))
      (begin
        ; A forward reference that the unit now defines is a definition from here on.
        (if (not (core-hash-ref (%env-origins env) sym #f))
          (begin
            (core-hash-put! (%env-origins env) sym (cons key sym))
            (core-hash-remove! (%env-cache env) sym)))
        var)
      (let ([fresh (make-variable)])
        (env-add! env sym fresh (cons key sym))
        fresh))))

;; Whether a local variable of ENV is what a lookup of its symbol finds.
(define (%env-local-visible? env local sym)
  (and local
    (or (variable-bound? local)
      (not (namespace? env))
      (and (core-hash-ref (%env-origins env) sym #f) #t))))

;; The binding of SYM in ENV: `(variable . origin)`, from a local one or else found
;; through the imports, or #f. Imported results are cached until the imports change.
(define (env-binding env sym)
  (let ([local (env-local-variable env sym)])
    (if (%env-local-visible? env local sym)
      (cons local (%env-origin-of env sym))
      (or (core-hash-ref (%env-cache env) sym #f)
        (let scan ([imports (%env-imports env)])
          (if (null? imports)
            #f
            (let ([binding (env-binding (car imports) sym)])
              (if binding
                (begin
                  (core-hash-put! (%env-cache env) sym binding)
                  binding)
                (scan (cdr imports))))))))))

;; The origin of the local binding of SYM: the owner of the variable, see slot 16.
(define (%env-origin-of env sym)
  (or (core-hash-ref (%env-origins env) sym #f)
    (cons (%env-key env) sym)))

;; The variable bound to SYM in ENV: a local one, else one found through the
;; imports.
(define (env-variable env sym)
  (let ([local (env-local-variable env sym)])
    (if (%env-local-visible? env local sym)
      local
      (let ([binding (env-binding env sym)])
        (and binding (car binding))))))

;; The origin `(key . sym)` of the binding of SYM in ENV, or #f when it has none: the
;; key of the linklet whose variable it is and the name it has there. Expansion uses it
;; to refer to the variable by its owner's slot.
(define (env-origin env sym)
  (let ([binding (env-binding env sym)])
    (and binding (cdr binding))))

;; The value bound to SYM in ENV.
(define (env-ref env sym . default)
  (let ([var (env-variable env sym)])
    (if (and var (variable-bound? var))
      (variable-ref var)
      (if (null? default)
        (assertion-violation 'env-ref "unbound variable" (%env-key env) sym)
        (car default)))))

;; Whether SYM is bound in ENV, locally or through imports.
(define (env-bound? env sym)
  (let ([var (env-variable env sym)])
    (and var (variable-bound? var))))

;; The environment named by KEY, where #f stands for the one being expanded in. A
;; library that is not loaded yet is loaded.
(define (global-environment key)
  (let ([cur (expansion-env)])
    (if (or (not key) (equal? key (%env-key cur)))
      cur
      (resolve-env key #t #t))))

;; The variable of SYM in the environment named by KEY (see `global-environment`), or
;; in its public interface when PUBLIC? is true. An unbound variable is made when
;; there is none yet.
(define (env-global-variable key sym public?)
  (let* ([env (global-environment key)]
         [found (env-variable (if public? (env-public-interface env) env) sym)])
    (or found (env-ensure-local-variable! env sym))))

;;; Mutation of the local table

;; Define SYM in ENV, keeping the existing variable when there is one.
(define (env-define! env sym val)
  (let ([var (env-local-variable env sym)])
    (if var
      (variable-set! var val)
      (env-add! env sym (make-variable val)))))

;; Bind SYM to the variable VAR in ENV, replacing any local entry. Imports
;; are shadowed by local bindings, so the cache entry for SYM is dropped to
;; keep it from going stale. ORIGIN, when given, is the `(key . sym)` of the owner
;; of VAR (see `env-origin`); without it VAR is taken to be defined by ENV.
(define (env-add! env sym var . origin)
  (core-hash-put! (%env-locals env) sym var)
  (core-hash-put! (%env-origins env) sym
    (if (pair? origin) (car origin) (cons (%env-key env) sym)))
  (core-hash-remove! (%env-cache env) sym))

;; Forget the local binding of SYM in ENV.
(define (env-remove! env sym)
  (core-hash-remove! (%env-locals env) sym)
  (core-hash-remove! (%env-origins env) sym)
  (core-hash-remove! (%env-cache env) sym))

;; Call PROC with each symbol and variable in ENV's own table.
(define (env-for-each proc env)
  (for-each (lambda (kv) (proc (car kv) (cdr kv)))
    (core-hash->list (%env-locals env))))

;; The local symbols of ENV whose variable is VAR.
(define (env-local-symbols-for-variable env var)
  (let ([out '()])
    (env-for-each
      (lambda (sym var*)
        (if (eq? var var*)
          (set! out (cons sym out))))
      env)
    out))

;;; Imports

;; The interface environments ENV searches after its own table.
(define (env-imports env) (%env-imports env))

;; Add IFACE to the imports of ENV unless it is already there (or is ENV).
(define (env-use-interface! env iface)
  (if (not (or (eq? env iface)
            (memq iface (%env-imports env))))
    (begin
      (%env-set-imports! env (cons iface (%env-imports env)))
      (core-hash-clear! (%env-cache env)))))

;; Add every interface in IFACES to the imports of ENV. Interfaces already
;; used are skipped, and an interface whose bindings are all provided by the
;; current imports is dropped, so a later import cannot silently shadow an
;; earlier one.
(define (env-use-interfaces! env ifaces)
  ;; The variable SYM provided by the first interface in IFACES.
  (define (find-provider ifaces sym)
    (let scan ([rest ifaces])
      (cond
        [(null? rest) #f]
        [(env-local-variable (car rest) sym)]
        [else (scan (cdr rest))])))

  ;; A copy of IFACE with only the bindings PRIOR does not already provide.
  ;; #f when IFACE contributes nothing new.
  (define (trim-against iface prior)
    (let ([trimmed #f]
          [overlapped? #f]
          [fresh? #t])
      (define (ensure-trimmed!)
        (or trimmed
          (let ([copy (make-env (%env-key iface) 'interface)])
            (%env-set-source! copy iface)
            (set! trimmed copy)
            copy)))
      (env-for-each
        (lambda (sym var)
          (let ([existing (find-provider prior sym)])
            (cond
              [(not existing)
                (set! fresh? #f)
                (env-add! (ensure-trimmed!) sym var (env-origin iface sym))]
              [(eq? existing var)
                (set! overlapped? #t)]
              [else
                (set! fresh? #f)
                (env-add! (ensure-trimmed!) sym var (env-origin iface sym))])))
        iface)
      (cond
        [fresh? #f]
        [overlapped? trimmed]
        [else iface])))

  (let* ([cur (%env-imports env)]
         [new (let collect ([in ifaces]
                            [accepted cur]
                            [accepted-explicit cur]
                            [out '()])
               (if (null? in)
                 (reverse out)
                 (let ([iface (car in)])
                   (if (or (memq iface accepted) (memq iface out))
                     (collect (cdr in) accepted accepted-explicit out)
                     (let ([trimmed (trim-against iface accepted-explicit)])
                       (if trimmed
                         (collect (cdr in)
                           (cons trimmed accepted)
                           (cons trimmed accepted-explicit)
                           (cons trimmed out))
                         (collect (cdr in) accepted accepted-explicit out)))))))])
    (%env-set-imports! env (append new cur))
    (core-hash-clear! (%env-cache env))))

;;; Public interface

;; The public interface of ENV, created on first use as an interface
;; environment with the same key. An interface is its own public interface.
(define (env-public-interface env)
  (cond
    [(eq? (%env-kind env) 'interface) env]
    [(%env-public env)]
    [else
      (let ([public (make-env (%env-key env) 'interface)])
        (%env-set-public! env public)
        public)]))

;; Export NAMES from ENV through its public interface. A name is SYM or
;; (internal . external). REPLACE? marks the exports as replacements that
;; override bindings from used interfaces.
(define (env-export! env names . replace?)
  (let ([replace? (and (pair? replace?) (car replace?))])
    (for-each
      (lambda (name)
        (let* ([internal (if (pair? name) (car name) name)]
               [external (if (pair? name) (cdr name) name)]
               [var (env-ensure-local-variable! env internal)]
               [public (env-public-interface env)])
          (if replace?
            (core-hash-put! (%env-replacements public) external #t))
          (env-note-export! env external internal replace?)
          (env-add! public external var (env-origin env internal))))
      names)))

;; Mark NAMES as replacements: `env-export!` with replace set.
(define (env-replace! env names)
  (env-export! env names #t))

;; Re-export NAMES from ENV. A name whose variable is local is exported
;; directly; any other name is exported as the variable it refers to.
(define (env-re-export! env names . replace?)
  (let ([replace? (and (pair? replace?) (car replace?))])
    (let ([public (env-public-interface env)])
      (for-each
        (lambda (name)
          (let* ([internal (if (pair? name) (car name) name)]
                 [external (if (pair? name) (cdr name) name)]
                 [var (env-variable env internal)])
            (cond
              [(not var)
                (env-export! env (list name) replace?)]
              [(eq? var (env-local-variable env internal))
                (env-export! env (list name) replace?)]
              [else
                (if replace?
                  (core-hash-put! (%env-replacements public) external #t))
                (env-note-export! env external internal replace? (%binding-kind var))
                (env-add! public external var (env-origin env internal))])))
        names))))

;; The replacement marks of ENV: a table from symbol to #t.
(define (env-replacements env) (%env-replacements env))

;;; Unique names

;; The next unique id of ENV
(define (env-generate-unique-id! . env)
  (let* ([e (if (null? env) (expansion-env) (car env))]
         [id (%env-unique-id e)])
    (%env-set-unique-id! e (+ id 1))
    id))

;; A fresh symbol `<id>-<hash>-<n>`, where HASH is `equal-hash` of the key of
;; ENV and N is the next unique id.
(define (env-gensym . args)
  (let* ([id (if (and (pair? args) (car args)) (car args) " mg")]
         [env (if (and (pair? args) (pair? (cdr args)))
                (cadr args)
                (expansion-env))]
         [hash (equal-hash (%env-key env))]
         [unique (env-generate-unique-id! env)])
    (string->symbol
      (string-append id "-" (number->string hash) "-" (number->string unique)))))

;; All environments reachable from ENV through imports, ENV first.
(define (env-and-imports env)
  (let lp ([in (list env)] [out '()])
    (cond
      [(null? in) (reverse out)]
      [(memq (car in) out) (lp (cdr in) out)]
      [else (lp (append (%env-imports (car in)) (cdr in))
              (cons (car in) out))])))

;; The symbols of the public interface of ENV whose variable is VAR, or '()
;; when ENV has no public interface yet.
(define (env-export-symbols-for-variable env var)
  (let ([public (if (eq? (%env-kind env) 'interface) env (%env-public env))])
    (if public
      (env-local-symbols-for-variable public var)
      '())))

;;; Registry of library environments

;; The table of library key -> environment of the current registry.
(define (env-registry) (registry-envs (current-library-registry)))

;; Libraries loaded from compiled code have no environment until something expands
;; against them. `lazy-envs` maps the key of such a library to (meta . instance), in the
;; current registry.
(define (lazy-envs) (registry-lazy-envs (current-library-registry)))


;; The environment registered under KEY, or #f.
(define (lookup-env key)
  (or (core-hash-ref (env-registry) key #f)
    (env-force-lazy! key)))

;; The environment registered under KEY, or #f. Never builds a lazy environment.
(define (lookup-env-registered key)
  (core-hash-ref (env-registry) key #f))

;; Register ENV under KEY, replacing any previous entry, and return ENV.
(define (register-env! key env)
  (core-hash-put! (env-registry) key env)
  env)

;; Whether KEY names a library every registry shares: `(capy)`, `(#%capy)`, the boot libraries
;; and the three libraries boot/libraries.scm defines, `(core intrinsics)`,
;; `(core primitives)` and `(core syntax-case)`. Those cannot be loaded from a file, and are
;; immutable.
(define (core-library-key? key)
  (and (pair? key)
    (or (memq (car key) '(boot |#%capy|))
      (equal? key '(capy))
      (member key '((core intrinsics) (core primitives) (core syntax-case))))
    #t))

;; A new registry named NAME that starts out with the libraries of PARENT that INHERIT? (a
;; procedure of a library key) accepts, instances and environments, and loads the other
;; libraries with RESOLVER, a procedure of a library key, or not at all when RESOLVER is #f (a
;; sealed registry). INHERIT? is `core-library-key?` unless it is given: the core libraries,
;; which cannot be loaded from a file and are immutable. What is inherited is shared with the
;; parent (the instance of a library, and its environment, are the ones of the parent), so a
;; library that something is going to define again must not be.
(define (make-child-library-registry name parent resolver . maybe-inherit?)
  (let* ([inherit? (if (pair? maybe-inherit?) (car maybe-inherit?) core-library-key?)]
         [reg (make-library-registry name parent resolver)])
    (for-each
      (lambda (key)
        (if (and (inherit? key) (not (library-instance reg key)))
          (register-library-instance! reg key (library-instance parent key))))
      (registered-library-keys parent))
    (for-each
      (lambda (entry)
        (if (inherit? (car entry))
          (core-hash-put! (registry-envs reg) (car entry) (cdr entry))))
      (core-hash->alist (registry-envs parent)))
    (for-each
      (lambda (entry)
        (if (inherit? (car entry))
          (core-hash-put! (registry-lazy-envs reg) (car entry) (cdr entry))))
      (core-hash->alist (registry-lazy-envs parent)))
    reg))

;; The environment registered under KEY, creating a library environment if
;; there is none.
(define (ensure-env key)
  (or (lookup-env key)
    (register-env! key (make-env key 'library))))

;; Loads the library KEY unless its environment is defined already. Every autoload of a
;; library goes through here: the library manager loads it with `linker-require!`.
(define (env-autoload! key)
  (if (not (env-registered-defined? key)) ($load-library-hook key)))

;; A hook called with KEY when `resolve-env-interface` finds no environment
;; for it, so the loader can load the library. By default it autoloads the
;; module of that name, and the loader registers the environment as it loads
;; the library. The hook's return value is ignored; the registry is consulted
;; afterwards. A #f hook disables autoloading.
(define env-autoload-hook
  (make-parameter env-autoload!))

;; Whether ENV has been defined: it has bindings or a public interface. An
;; environment that exists only because something looked it up has not.
(define (env-defined? env)
  (or (%env-public env)
    (> (core-hash-size (%env-locals env)) 0)))

;; The environment for KEY with its library loaded: it is looked up, and when
;; there is none (or it is empty) the hook loads the library first.
(define (env-for-key key)
  (let ([env (lookup-env key)])
    (if (not (and env (env-defined? env)))
      (let ([hook (env-autoload-hook)])
        (if hook (hook key))))
    (let ([env (ensure-env key)])
      env)))

;;; Library views
;;;
;;; A package writes the names of the libraries it uses with the prefixes its dependencies
;;; give them, and not with the keys the libraries have (see "Library keys of packages" in
;;; linker.scm). The view of the package says which key a name stands for. The expander maps
;;; every library name it resolves, in an import and in a library form, with the view that is
;;; current; with no view (the default) a name is its own key.

(define current-library-view (make-parameter #f))

;; The key of the library the current view sees under NAME. It is an error when the view sees
;; no such library.
(define (library-view-key name)
  (let ([view (current-library-view)])
    (or (linker-view-key view name)
      (error-without-irritants ($library-not-found-message-hook name)))))

;; The public interface of the environment for KEY, optionally restricted by
;; SELECT (a list of symbols or (orig . seen) specs), HIDE (names to leave
;; out), and PREFIX (a symbol added to every exported name). The optional VERSION-REFERENCE
;; is the R6RS version reference of the import, checked against the version of the library.
;; KEY is canonicalized by the linker (see `linker-canonical-key`).
(define (resolve-env-interface key select hide prefix . version-reference)
  (let* ([key (linker-canonical-key key)]
         [env (env-for-key key)]
         [public (begin (env-note-dep! key) (%env-public env))]
         [renamer (if prefix
                    (lambda (sym) (symbol-append prefix sym))
                    (lambda (sym) sym))])
    (if (and public (pair? version-reference) (pair? (car version-reference)))
      (check-import-version! key (%env-version env) (car version-reference)))
    ;; A missing library is reported, and the import binds nothing: the error
    ;; returns to the caller, so the empty interface stands in for the library.
    (cond
      [(not public)
        (error-without-irritants ($library-not-found-message-hook key))
        (make-env key 'interface)]
      [(and (not select) (null? hide) (not prefix)) public]
      [else
      (let ([custom (make-env key 'interface)])
        (define (export-binding! src dst var)
          (if (not (memq src hide))
            (let ([renamed (renamer dst)])
              (if (core-hash-ref (%env-replacements public) src #f)
                (core-hash-put! (%env-replacements custom) renamed #t))
              (env-add! custom renamed var (env-origin public src)))))

        (for-each (lambda (binding)
                   (if (not (env-local-variable public binding))
                     (assertion-violation #f "no binding to hide in module" key binding)))
          hide)

        (cond
          [select
            (for-each (lambda (bspec)
                       (let* ([direct? (symbol? bspec)]
                              [orig (if direct? bspec (car bspec))]
                              [seen (if direct? bspec (cdr bspec))]
                              [var (env-local-variable public orig)])
                         (if (not var)
                           (assertion-violation 'unbound-variable
                             "no binding to select in module"
                             orig key))
                         (export-binding! orig seen var)))
              select)]
          [else
            (env-for-each (lambda (sym var)
                            (export-binding! sym sym var))
              public)])
        custom)])))

;;; Library decls: what a compiled library tells the loader
;;;
;;; A library linklet carries its declaration in its meta, so the environment of the library
;;; can be rebuilt from the decl and the instance without running the library body. The decl is
;;; a vector with a tag, since it has to be plain data that survives a FASL round trip (the
;;; directory header of a FASL file holds it, and `fasl-read-directory` reads it without
;;; running any code). This is the one place that knows its layout; everything else uses the
;;; accessors below.
;;;
;;;   #(#%library key version exports imports language replacements
;;;     source-hash interface-hash deps compiler-id knowns compile-deps)
;;;
;;;   key           the library name, a list of symbols and integers
;;;   version       the version of the library form, a list of integers (maybe empty)
;;;   exports       ((external internal kind) ...), ordered by external name, exactly what the export spec of the library
;;;                 says: EXTERNAL is the name importers see, INTERNAL the name the library
;;;                 binds (a definition of the instance, or a binding of an import, a
;;;                 re-export). KIND is `variable` or `macro`, as the expander bound it
;;;   imports       ((mode . import-set) ...) in the order they were imported. MODE is the
;;;                 execution mode the import set was read in (`r7rs`, `r6rs` or the
;;;                 permissive `compat`)
;;;   language      the execution mode the library was declared in
;;;   replacements  the external names the library declares as replacements, with a
;;;                 `(replace name ...)` export spec: exports that stand for a binding the
;;;                 importers may have already. Only what the library says is listed. Whether a
;;;                 name is also bound in the environment the library was compiled in is not
;;;                 looked at, so the decl does not depend on the process that made it
;;;   source-hash   the hash of the bytes of the source file the library was compiled from (the
;;;                 whole file, when it holds several libraries), or #f when it has none
;;;   interface-hash  the hash of what importers can depend on, see `library-interface-hash`
;;;   deps          ((key . interface-hash) ...) for the libraries the library imported, the
;;;                 hashes being the ones of the libraries when it was compiled
;;;   compiler-id   the id of the compiler that made the decl (`%compiler-id`): the format
;;;                 hash of the bytecode and primitives, and the version of capy
;;;   knowns        ((internal . known) ...) for the exported variables the compiler knows
;;;                 something about: `(constant datum)` for a variable defined once as an
;;;                 immediate literal and never assigned, `(procedure arity-mask)` for one
;;;                 defined once as a `lambda` and never assigned. The mask has bit N set when
;;;                 the procedure takes N arguments, and is negative when it takes any number
;;;                 from some N on. A library that defines syntax has no knowns of its own.
;;;                 A re-exported variable has the known of its origin (`env-reexport-knowns`).
;;;   compile-deps  ((path . hash) ...) (or (native-key . identity) for an extension) for the files the expansion of the library read besides
;;;                 its source: the files it `include`s, the headers `c-include` parsed (see
;;;                 `note-compile-dependency!`). PATH is an absolute path, a string, and HASH the
;;;                 hash of the bytes of the file when it was read, or #f when it did not
;;;                 exist. Sorted by path.
;;;
;;; The hashes are what makes a compiled library reusable (`fasl-needs-recompile?` in the
;;; linker): a compiled library is recompiled when the compiler or its source differs from the
;;; ones in its decl, or when a library it imports now has another interface hash. A change of
;;; the body of a library that does not change its interface hash does not recompile the
;;; libraries that import it, and a change of the knowns does, since an importer has folded
;;; them into its code. A compiled library is recompiled too when one of its compile-deps has
;;; another hash now: the file it read changed, and its code may depend on what is in it.
;;;
;;; The meta of a namespace linklet is `(#%namespace key . imports)`, where IMPORTS are the
;;; import entries the unit ran in the namespace. The meta of a boot linklet is `(#%boot key)`.

(define library-decl-length 13)

(define (make-library-decl key version exports imports language replacements
          source-hash interface-hash deps compiler-id knowns compile-deps)
  (vector '|#%library| key version exports imports language replacements
    source-hash interface-hash deps compiler-id knowns compile-deps))

(define (library-decl? x)
  (and (vector? x)
    (= (vector-length x) library-decl-length)
    (eq? (vector-ref x 0) '|#%library|)))

(define (library-decl-key decl) (vector-ref decl 1))
(define (library-decl-version decl) (vector-ref decl 2))
(define (library-decl-exports decl) (vector-ref decl 3))
(define (library-decl-imports decl) (vector-ref decl 4))
(define (library-decl-language decl) (vector-ref decl 5))
(define (library-decl-replacements decl) (vector-ref decl 6))
(define (library-decl-source-hash decl) (vector-ref decl 7))
(define (library-decl-interface-hash decl) (vector-ref decl 8))
(define (library-decl-deps decl) (vector-ref decl 9))
(define (library-decl-compiler-id decl) (vector-ref decl 10))
(define (library-decl-knowns decl) (vector-ref decl 11))
(define (library-decl-compile-deps decl) (vector-ref decl 12))

(define (export-entry-external entry) (car entry))
(define (export-entry-internal entry) (cadr entry))
(define (export-entry-kind entry) (caddr entry))

;;; What the expander notes about the library it is compiling

(define (env-begin-unit! env version)
  (%env-set-version! env version)
  (%env-set-language! env (if $execution-mode-hook ($execution-mode) 'compat))
  (%env-set-export-log! env '())
  (%env-set-import-log! env '())
  (%env-set-deps! env '())
  (core-hash-clear! (%env-macros env)))

;; The kind of what VAR is bound to, by the expander's own test of a macro binding.
(define (%binding-kind var)
  (if (and var (variable-bound? var) (macro? (variable-ref var)))
    'macro
    'variable))

;; Note that ENV exports INTERNAL as PUBLIC. KIND is the kind of the binding when it is known
;; already: a re-export takes it from the binding it re-exports. For a definition of the
;; library it is left out, and the kind is the one the expander noted for the definition
;; (`env-note-macro!`) when the decl is made.
(define (env-note-export! env public internal replace? . kind)
  (%env-set-export-log! env
    (cons (list public internal replace? (and (pair? kind) (car kind)))
      (%env-export-log env))))

;; Note that the library of ENV defines INTERNAL as syntax (`define-syntax`), or as a variable
;; again (`define`).
(define (env-note-macro! env internal)
  (core-hash-put! (%env-macros env) internal #t))

(define (env-note-variable! env internal)
  (core-hash-remove! (%env-macros env) internal))

;; Whether the library of ENV defines any syntax.
(define (env-defines-macros? env)
  (> (core-hash-size (%env-macros env)) 0))

;; Record the import set SPEC that was imported into ENV, and note that it is applied.
(define (env-record-import! env spec)
  (let ([entry (cons ($execution-mode) spec)])
    (%env-set-import-log! env (cons entry (%env-import-log env)))
    (%env-set-applied! env (cons entry (%env-applied env)))))

;; The import entries recorded in ENV since the last call, oldest first.
(define (env-take-unit-imports! env)
  (let ([imports (let loop ([in (reverse (%env-import-log env))] [out '()])
                   (cond
                     [(null? in) (reverse out)]
                     [(member (car in) out) (loop (cdr in) out)]
                     [else (loop (cdr in) (cons (car in) out))]))])
    (%env-set-import-log! env '())
    imports))

;; Whether imports were recorded in ENV that no unit has taken yet.
(define (env-unit-imports? env)
  (pair? (%env-import-log env)))

;; Calls THUNK with no import recorded in ENV, and records again afterwards the imports that were
;; pending before. A file compiled in a namespace takes the imports made in the namespace into
;; its namespace linklet (`env-take-unit-imports!`), but only the ones its own forms make are
;; its imports: the ones the user made before (`-c '(import ...)'`) belong to the unit of the
;; user, and a compiled file that held them would depend on what was imported before it.
(define (call-with-own-imports env thunk)
  (if (and env (env? env))
    (let ([pending #f])
      (dynamic-wind
        (lambda ()
          (set! pending (%env-import-log env))
          (%env-set-import-log! env '()))
        thunk
        (lambda ()
          (%env-set-import-log! env pending))))
    (thunk)))

;; The internal names the library of ENV exports, as noted so far.
(define (env-export-internals env)
  (map cadr (%env-export-log env)))

;; The export notes of ENV, the newest note of an external name standing: a list of
;; `(external internal replace? kind)`, in the order of the notes.
(define (env-export-entries env)
  (let ([seen (make-core-hash-eq)])
    ; Newest first, so the first entry of an external name is the one that stays.
    (let loop ([log (%env-export-log env)] [out '()])
      (cond
        [(null? log) out]
        [(core-hash-ref seen (car (car log)) #f) (loop (cdr log) out)]
        [else
          (core-hash-put! seen (car (car log)) #t)
          (loop (cdr log) (cons (car log) out))]))))

;; The origin `(key . sym)` of SYM as the imports of ENV bind it, or #f. A variable the library
;; exports without defining it (a name an import binds) has a local variable in ENV from the
;; export, which the lookup would find first: it is the imports that tell where the name comes
;; from, as they do when the library is loaded again from its decl (`env-refresh-exports!`).
;; The libraries of the runtime itself (`core-library-key?`) say nothing: `(capy)` is the core, whose
;; bindings change as the libraries are loaded (each one mirrors its exports into it), and
;; the others are bound through it, so what they bind depends on what the compiling process
;; has loaded, and the decl would too. They are not compiled by themselves, so a name they
;; bind cannot move. A trimmed import (see `env-use-interfaces!`) is read as the interface it
;; was copied from: what the copy leaves out is what the imports before it bind, and `(capy)`
;; binds more as the process loads libraries, so the copy depends on what was loaded.
(define (env-imported-origin env sym)
  (let scan ([imports (%env-imports env)])
    (and (pair? imports)
      (let* ([iface (or (%env-source (car imports)) (car imports))]
             [binding (and (not (core-library-key? (%env-key iface)))
                        (env-binding iface sym))])
        (if binding
          (let ([origin (cdr binding)])
            (and (not (equal? (car origin) (%env-key env))) origin))
          (scan (cdr imports)))))))

;; The exports of the library in ENV as the decl lists them:
;; `((external internal kind origin) ...)`, ordered by the external name. ORIGIN is #f for a
;; binding the library defines, and the origin `(key . name)` of the binding for a re-export:
;; importers refer to a re-exported variable through the library that defines it, so where it
;; comes from is part of the interface. The order the library form noted them in has no meaning, and
;; the decl is the same for the same library however its export specs were sorted out.
(define (env-library-exports env . defined)
  (define defined-set (and (pair? defined) (env-symbol-set (car defined))))
  (sort-by-string
    (map (lambda (entry)
          (list (car entry) (cadr entry)
            (or (cadddr entry)
              (if (core-hash-ref (%env-macros env) (cadr entry) #f) 'macro 'variable))
            (and defined-set
              (not (core-hash-ref defined-set (cadr entry) #f))
              (not (core-hash-ref (%env-macros env) (cadr entry) #f))
              (env-imported-origin env (cadr entry)))))
      (env-export-entries env))
    (lambda (entry) (symbol-key (car entry)))))

;; The external names the library in ENV exports with `export!`, ordered by name.
(define (env-library-replacements env)
  (sort-by-string
    (let loop ([es (env-export-entries env)] [out '()])
      (cond
        [(null? es) (reverse out)]
        [(caddr (car es)) (loop (cdr es) (cons (car (car es)) out))]
        [else (loop (cdr es) out)]))
    symbol-key))

;;; Hashes of a library

;; The source hash of the file being compiled (a string), or #f. The file compiler sets it
;; (see `call-with-source-hash`) before it reads the file, so a file that changes while it is
;; compiled has the hash of the older text and is recompiled the next time.
(define current-source-hash (make-parameter #f))

(define (call-with-source-hash hash source thunk)
  (let ([saved #f] [saved-deps '()] [saved-directory #f])
    (dynamic-wind
      (lambda ()
        (set! saved (current-source-hash))
        (set! saved-deps compile-dependencies)
        (set! saved-directory compile-dependency-directory)
        (set! compile-dependencies '())
        (set! compile-dependency-directory (source-directory source))
        (current-source-hash hash))
      thunk
      (lambda ()
        (set! compile-dependencies saved-deps)
        (set! compile-dependency-directory saved-directory)
        (current-source-hash saved)))))

;; The hash of the source file PATH as the compiler records it in a decl, or #f when it cannot be read.
(define (source-file-hash path) (%file-hash path))

;;; The output directory and the compile-time dependencies
;;;
;;; Code that generates code while a library expands (`c-include`) writes it into
;;; `(compile-output-directory)`, as the libraries of the package that is being compiled (the
;;; `OUT_DIR` of Cargo). `capy build` gives every package one, `build/<package>/out` below the
;;; target directory of the build, and passes it with `--compile-output-directory`. Outside a
;;; package it is `$XDG_CACHE_HOME/capy/out`, or `~/.cache/capy/out`.
;;;
;;; The files that the expansion of a library read (the ones it includes, the headers it
;;; parsed) are its compile-time dependencies: `(note-compile-dependency! path)` records the
;;; path and the hash of the file, and the decl of the library that is being closed takes the
;;; ones noted since the previous decl was (`take-compile-dependencies!`). A compiled library
;;; is stale when one of the files has another hash now (`fasl-needs-recompile?`). The native
;;; extensions it loaded are noted too, as `(KEY . IDENTITY)` (`note-native-dependency!`).
;;;
;;; A file in the directory of the source being compiled, or below it, is recorded relative to
;;; that directory, and is found again from the source that the check is given. A compiled file
;;; then does not depend on where the checkout is: the same sources compiled in two places give
;;; the same bytes. Files elsewhere (system headers) are recorded by their absolute path.

(define (default-compile-output-directory)
  (let ([xdg (getenv "XDG_CACHE_HOME")] [home (getenv "HOME")])
    (cond
      [(and xdg (not (equal? xdg ""))) (string-append xdg "/capy/out")]
      [else (string-append (if (and home (not (equal? home ""))) home ".") "/.cache/capy/out")])))

(define compile-output-directory (make-parameter (default-compile-output-directory)))

(linker-set-output-directory-hook! (lambda () (compile-output-directory)))

;; ((path . hash) ...) noted since the start of the file being compiled, newest first.
(define compile-dependencies '())

;; The canonical directory of the source being compiled, ending in `/`, or #f.
(define compile-dependency-directory #f)

;; The canonical directory of the file SOURCE, ending in `/`, or #f when SOURCE is not a file.
(define (source-directory source)
  (and (string? source) (file-exists? source)
    (let ([path (canonicalize-path-string source)])
      (let loop ([i (- (string-length path) 1)])
        (cond
          [(< i 0) #f]
          [(char=? (string-ref path i) #\/) (substring path 0 (+ i 1))]
          [else (loop (- i 1))])))))

;; PATH as it is recorded: relative to DIRECTORY when it is below it.
(define (compile-dependency-name path directory)
  (let ([n (and directory (string-length directory))])
    (if (and n (> (string-length path) n) (string=? (substring path 0 n) directory))
      (substring path n (string-length path))
      path)))

;; The file the recorded NAME is, for the library whose source is SOURCE: NAME itself when it is
;; absolute, else NAME in the directory of SOURCE, or #f when SOURCE gives no directory.
(define (compile-dependency-file name source)
  (if (and (> (string-length name) 0) (char=? (string-ref name 0) #\/))
    name
    (let ([directory (source-directory source)])
      (and directory (string-append directory name)))))

;; Notes that the expansion of the library being compiled read the file PATH. It does nothing
;; when no file is being compiled.
(define (note-compile-dependency! path)
  (if (and (string? path) (current-source-hash))
    (let* ([file (if (file-exists? path) (canonicalize-path-string path) path)]
           [name (compile-dependency-name file compile-dependency-directory)])
      (let loop ([deps compile-dependencies])
        (cond
          [(null? deps)
            (set! compile-dependencies (cons (cons name (%file-hash file)) compile-dependencies))]
          [(equal? (car (car deps)) name) #f]
          [else (loop (cdr deps))])))))

;; Makes the library NAME, a library `c-include` (or any code that generates one) wrote into
;; `(compile-output-directory)` as the file SOURCE, known to the loader of this process as the
;; library of the package being compiled, whose view gives the key. The libmap of the build does
;; not list it before it is compiled. A library the libmap lists keeps its entry.
(define (register-generated-library! name source)
  (linker-libmap-register-source! (library-view-key name)
    (if (file-exists? source) (canonicalize-path-string source) source)))

;; Notes that the expansion of the library being compiled loaded the native extension KEY
;; (`(#%native <package-id> <name>)`) whose identity is IDENTITY: the entry is `(KEY . IDENTITY)`
;; instead of `(PATH . HASH)`. An extension is not a library the one that is compiled imports, so
;; it has no interface hash in `deps`; it is something the expansion read, like a header. It does
;; nothing when no file is being compiled. The linker calls it when a library loads an extension
;; by key (`load-native-extension`).
(define (note-native-dependency! key identity)
  (if (current-source-hash)
    (let loop ([deps compile-dependencies])
      (cond
        [(null? deps)
          (set! compile-dependencies (cons (cons key identity) compile-dependencies))]
        [(equal? (car (car deps)) key) #f]
        [else (loop (cdr deps))]))))

(linker-set-native-hook! note-native-dependency!)

;; The string a compile-time dependency is sorted by: its path, or the text of its key.
(define (compile-dependency-sort-key dep)
  (let ([key (car dep)])
    (if (string? key)
      key
      (string-append "#native " (symbol->string (car (cdr key))) " " (symbol->string (car (cdr (cdr key))))))))

;; The dependencies noted so far, sorted by path, which are forgotten.
(define (take-compile-dependencies!)
  (let ([deps compile-dependencies])
    (set! compile-dependencies '())
    (sort-by-string deps compile-dependency-sort-key)))

;; The identity of the native extension KEY in the libmap that is installed, or #f.
(define (installed-native-identity key)
  (let ([entry (linker-native-entry key)])
    (and entry (car (cdr (cdr entry))))))

;; Whether every compile-time dependency of DEPS is as it was: a file has the hash it had, and
;; a native extension the identity it had. NATIVE-IDENTITY gives the current identity of a key.
;; SOURCE is the source of the library, which relative files are found from. Without it (a
;; library installed without its source) they are not checked, as the source hash is not.
(define (compile-dependencies-current?/native deps native-identity source)
  (let loop ([deps deps])
    (or (null? deps)
      (and (if (string? (car (car deps)))
            (let ([file (compile-dependency-file (car (car deps)) source)])
              (or (not file) (equal? (%file-hash file) (cdr (car deps)))))
            (equal? (native-identity (car (car deps))) (cdr (car deps))))
        (loop (cdr deps))))))

;; Whether the compile-time dependencies DEPS of the library whose source is SOURCE are as they
;; were, the extensions being the ones of the libmap that is installed.
(define (compile-dependencies-current? deps source)
  (compile-dependencies-current?/native deps installed-native-identity source))

;; ITEMS sorted by the string KEY-OF gives for them, as a new list. The sort is stable (items
;; with equal keys keep their order), and KEY-OF is called once per item: a merge sort, since a
;; library can export thousands of names.
(define (sort-by-string items key-of)
  ;; A and B are sorted lists of `(key . item)`; on equal keys the items of A come first
  (define (merge a b)
    (let loop ([a a] [b b] [out '()])
      (cond
        [(null? a) (append-reverse out b)]
        [(null? b) (append-reverse out a)]
        [(string<? (car (car b)) (car (car a))) (loop a (cdr b) (cons (car b) out))]
        [else (loop (cdr a) b (cons (car a) out))])))
  (define (append-reverse rev tail)
    (if (null? rev) tail (append-reverse (cdr rev) (cons (car rev) tail))))
  ;; one pass merging the runs pairwise, in order
  (define (merge-pairs runs)
    (cond
      [(null? runs) '()]
      [(null? (cdr runs)) runs]
      [else (cons (merge (car runs) (cadr runs)) (merge-pairs (cddr runs)))]))
  (let loop ([runs (map (lambda (item) (list (cons (key-of item) item))) items)])
    (cond
      [(null? runs) '()]
      [(null? (cdr runs)) (map cdr (car runs))]
      [else (loop (merge-pairs runs))])))

;; A set of the symbols SYMS, for `core-hash-ref`.
(define (env-symbol-set syms)
  (let ([table (make-core-hash-eq)])
    (for-each (lambda (sym) (core-hash-put! table sym #t)) syms)
    table))

(define (symbol-key sym) (symbol->string sym))

;; Note that the library of the expansion environment imports the library KEY, so its decl
;; records the interface hash it was compiled against.
(define (env-note-dep! key)
  (let ([env (expansion-env)])
    (if (and (eq? (%env-kind env) 'library) (not (equal? key (%env-key env))))
      (%env-set-deps! env (cons key (%env-deps env))))))

;; Whether the library KEY has an interface hash that a compiled importer depends on: not
;; the libraries of the runtime itself, which cannot be compiled again by themselves.
(define (hashed-dependency-key? key)
  (not (or (not (pair? key))
        (memq (car key) '(boot |#%capy|))
        (equal? key '(capy)))))

;; The decl of the library KEY, loaded or compiled in this registry, or #f.
(define (library-decl-of key)
  (let ([env (lookup-env key)])
    (and env (%env-decl env))))

;; The knowns of the boot units, `((name . knowns) ...)`: what the `(#%boot key knowns)` meta of
;; each one said when the linker ran it (see `boot-unit-knowns` in units.scm).
(define %boot-knowns '())

;; Records KNOWNS as the knowns of the boot unit NAME, called by the linker.
(define (env-boot-knowns! name knowns)
  (if (pair? knowns)
    (set! %boot-knowns (cons (cons name knowns) %boot-knowns))))

;; The knowns of the boot unit NAME, or '().
(define (env-boot-knowns name)
  (let ([entry (assoc name %boot-knowns)])
    (if entry (cdr entry) '())))

;; `((key . interface-hash) ...)` for the libraries KEYS (without duplicates, the library
;; SELF left out) that have an interface hash.
(define (library-dependency-hashes self keys)
  (let loop ([keys keys] [seen '()] [out '()])
    (cond
      [(null? keys) (reverse out)]
      [(or (member (car keys) seen) (equal? (car keys) self) (not (hashed-dependency-key? (car keys))))
        (loop (cdr keys) seen out)]
      [else
        (let* ([decl (library-decl-of (car keys))]
               [hash (and decl (library-decl-interface-hash decl))])
          (loop (cdr keys) (cons (car keys) seen)
            (if (string? hash) (cons (cons (car keys) hash) out) out)))])))

;; The interface hash of a library: the hash of everything an importer, or a compiled
;; importer, can depend on.
;;
;; - The version, the exports with their kinds (by external name, in a stable order, not
;;   the internal names, which an importer does not see), and the names exported with
;;   `export!`.
;; - The origin of each re-export, the library and name it is defined as there: importers
;;   refer to the variable through that library, so a re-export that moves to another
;;   library changes what they compile to.
;; - The knowns: an importer folds them into its code.
;; - A macro of the library can expand into anything at a use site, in particular into
;;   references to the definitions the library does not export. So when the library defines
;;   syntax or exports a macro (it may be a macro of another library), the hash also covers
;;   the source hash of the library when it defines syntax, and the interface hashes of the
;;   libraries it imports, which carry the source hashes of the libraries they define syntax
;;   in. The hash is #f when the library defines syntax and has no source hash: nothing
;;   holds importers of it fresh.
;;
;; A library that exports only variables and has no knowns has the same interface hash after
;; any change of its body.
(define (library-interface-hash version exports replacements knowns
          defines-syntax? source-hash deps)
  (let* ([macro-export? (let loop ([es exports])
                          (and (pair? es) (or (eq? (caddr (car es)) 'macro) (loop (cdr es)))))]
         [syntax? (or defines-syntax? macro-export?)]
         [sorted-exports (sort-by-string
                          (map (lambda (e) (list (car e) (caddr e) (cadddr e))) exports)
                          (lambda (e) (symbol-key (car e))))])
    (if (and defines-syntax? (not source-hash))
      #f
      (%datum-hash
        (list 'interface 1
          version
          sorted-exports
          (sort-by-string replacements symbol-key)
          (sort-by-string knowns (lambda (k) (symbol-key (car k))))
          (and defines-syntax? source-hash)
          (and syntax?
            (sort-by-string deps (lambda (d) (%datum-hash (car d))))))))))

;; The knowns of the variables the library in ENV re-exports, `((internal . known) ...)` in the
;; order of EXPORTS (its entries, see `env-library-exports`), DEFINED being the names it defines.
;; A re-export has the known of its origin: the one in the decl of the library that defines the
;; variable, or, for a variable of the runtime itself (which has no decl: `car`, `map`), the
;; arity of the procedure it holds, so a reader of the decl (a tool that shows what a name is)
;; sees what an importer of the origin would. The compiler does not read these: a reference to
;; a re-exported variable is to its origin, which has its own knowns.
(define (env-reexport-knowns env exports defined)
  (define defined-set (env-symbol-set defined))
  (define known-set (make-core-hash-eq))
  (let loop ([es exports] [out '()])
    (if (null? es)
      (reverse out)
      (let* ([entry (car es)]
             [internal (cadr entry)]
             [origin (cadddr entry)]
             [known
               (and (eq? (caddr entry) 'variable)
                 (not (core-hash-ref defined-set internal #f))
                 (let ([decl (and origin (library-decl-of (car origin)))])
                   (if decl
                     (let ([found (assq (cdr origin) (library-decl-knowns decl))])
                       (and found (cdr found)))
                     ;; no decl: the variable is the runtime's own, in ORIGIN's environment
                     (let* ([oenv (or (and origin (lookup-env (car origin))) env)]
                            [var (or (env-variable oenv (if origin (cdr origin) internal))
                                   (env-variable env internal))]
                            [value (and var (variable-bound? var) (variable-ref var))]
                            [mask (and (procedure? value) (procedure-arity-mask value))])
                       ;; A mask of -1 (any count) says nothing, and leaving it out keeps the decl
                       ;; the same whichever compiler built the runtime: the seed compiles a
                       ;; `case-lambda' as one variadic procedure (-1), the Scheme compiler as a
                       ;; multi-entry one (#f).
                       (and mask (not (eqv? mask -1)) (list 'procedure mask))))))])
        (loop (cdr es)
          (if (and known (not (core-hash-ref known-set internal #f)))
            (begin
              (core-hash-put! known-set internal #t)
              (cons (cons internal known) out))
            out))))))

;; The decl of the library in ENV, which is recorded in ENV: its exports are the ones noted
;; from its export spec, the newest note of an external name standing. KNOWNS are the knowns
;; of the library, IMPORT-KEYS the keys of the linklets its body refers to, which count
;; as imports besides the ones the library imported, and DEFINED the names its body defines
;; as variables.
(define (env-library-decl env knowns import-keys defined)
  (let* ([exports (env-library-exports env defined)]
         [replacements (env-library-replacements env)]
         [source-hash (current-source-hash)]
         [deps (library-dependency-hashes (%env-key env)
                (append (reverse (%env-deps env)) import-keys))]
         [knowns (append (if (env-defines-macros? env) '() knowns)
                   (env-reexport-knowns env exports defined))]
         [decl (make-library-decl
                (%env-key env)
                (%env-version env)
                exports
                (reverse (%env-import-log env))
                (%env-language env)
                replacements
                source-hash
                (library-interface-hash (%env-version env) exports replacements knowns
                  (env-defines-macros? env) source-hash deps)
                deps
                (%compiler-id)
                knowns
                (take-compile-dependencies!))])
    (%env-set-decl! env decl)
    decl))

;;; Is a compiled library fresh?

;; The library decls in the directory header of the FASL file PATH, read without running
;; anything: a list of the vectors that start with `#%library` (of any layout), or #f when the
;; file cannot be read, was made by another format of compiled code, or has no directory.
(define (fasl-library-decls path)
  (let ([directory (%fasl-directory path)])
    (and directory
      (let loop ([entries directory] [out '()])
        (if (null? entries)
          (reverse out)
          (let ([meta (and (list? (car entries)) (= (length (car entries)) 4)
                        (cadddr (car entries)))])
            (loop (cdr entries)
              (if (and (vector? meta) (> (vector-length meta) 0)
                   (eq? (vector-ref meta 0) '|#%library|))
                (cons meta out)
                out))))))))

;; The id of one compilation of a file that defines the libraries DECLS: the hash of what the
;; compiled code depends on, the source hash, the compiler id and the interface hashes of the
;; libraries it imports. A `.wpo` file records the id of the compiled file it was made with,
;; and is ignored when the compiled file has another (see `wpo-linklets` in eval.scm).
(define (library-compile-id decls)
  (%datum-hash
    (list 'compile 1
      (map (lambda (decl)
            (list (library-decl-key decl) (library-decl-source-hash decl)
              (library-decl-compiler-id decl) (library-decl-deps decl)))
        decls))))

;; The decl of the library KEY among DECLS, or #f.
(define (find-library-decl decls key)
  (let loop ([decls decls])
    (cond
      [(null? decls) #f]
      [(and (library-decl? (car decls)) (equal? (library-decl-key (car decls)) key)) (car decls)]
      [else (loop (cdr decls))])))

;; Whether the compiled library file FASL-PATH has to be compiled again from SOURCE-PATH:
;;
;; - the file cannot be read, or the compiler that made it is not this one, or
;; - the hash of the source differs from the one in the file, or
;; - a file its expansion read has another hash now (its compile-deps), or
;; - a library it imported has another interface hash now than when it was compiled.
;;
;; RESOLVER is a procedure of a library key. It returns the path of a compiled file of the
;; library that is current (compiling the library when it has to), whose directory header
;; holds the library as it is now, or #f when there is no such library. The interface hash
;; of an imported library is read from that file, so a change of the body of the imported
;; library that does not change its interface hash does not make this library stale.
;;
;; A file that holds no library (a script, a boot file) has no hashes. It is stale when its
;; source was modified after it.
(define (fasl-needs-recompile? fasl-path source-path resolver)
  (let ([decls (fasl-library-decls fasl-path)])
    (cond
      [(not decls) #t]
      [(null? decls) (%file-newer? source-path fasl-path)]
      [else
        (let ([source-hash (source-file-hash source-path)]
              [compiler-id (%compiler-id)])
          (or (not source-hash)
            (let loop ([ds decls])
              (cond
                [(null? ds) #f]
                [(not (library-decl? (car ds))) #t]
                [(not (equal? (library-decl-compiler-id (car ds)) compiler-id)) #t]
                [(not (equal? (library-decl-source-hash (car ds)) source-hash)) #t]
                [(not (compile-dependencies-current? (library-decl-compile-deps (car ds)) source-path)) #t]
                [(library-dependencies-changed? (car ds) decls resolver) #t]
                [else (loop (cdr ds))]))))])))

;; Whether a library DECL imported has another interface hash now than the one DECL records.
;; The libraries the file of DECL (DECLS) defines itself are compiled together with it.
(define (library-dependencies-changed? decl decls resolver)
  (let loop ([deps (library-decl-deps decl)])
    (cond
      [(null? deps) #f]
      [(find-library-decl decls (car (car deps))) (loop (cdr deps))]
      [else
        (let* ([path (resolver (car (car deps)))]
               [current (and path (fasl-library-decls path))]
               [dep-decl (and current (find-library-decl current (car (car deps))))])
          (if (and dep-decl
               (library-decl-interface-hash dep-decl)
               (equal? (library-decl-interface-hash dep-decl) (cdr (car deps))))
            (loop (cdr deps))
            #t))])))

;; The meta of the linklet of a unit that ran in the namespace ENV
(define (namespace-meta env)
  (cons '|#%namespace| (cons (env-key env) (env-take-unit-imports! env))))

;;; Versions
;;;
;;; An import may carry an R6RS version reference. `(version-reference-matches? ref version)`
;;; is the matching of R6RS 7.1:
;;;
;;;   version-reference  ::= (sub-version-reference ...) | (and version-reference ...)
;;;                        | (or version-reference ...) | (not version-reference)
;;;   sub-version-reference ::= sub-version | (>= sub-version) | (<= sub-version)
;;;                        | (and sub-version-reference ...) | (or sub-version-reference ...)
;;;                        | (not sub-version-reference)
;;;
;;; A list of sub-version references matches a version when each one matches the sub-version
;;; at its position; the version needs at least as many sub-versions, and the rest is ignored.

(define (sub-version-reference-matches? ref v)
  (cond
    [(exact-integer? ref) (= ref v)]
    [(and (pair? ref) (list? ref))
      (case (car ref)
        [(>=) (and (= (length ref) 2) (>= v (cadr ref)))]
        [(<=) (and (= (length ref) 2) (<= v (cadr ref)))]
        [(and) (let loop ([r (cdr ref)])
                 (or (null? r) (and (sub-version-reference-matches? (car r) v) (loop (cdr r)))))]
        [(or) (let loop ([r (cdr ref)])
                (and (pair? r) (or (sub-version-reference-matches? (car r) v) (loop (cdr r)))))]
        [(not) (and (= (length ref) 2) (not (sub-version-reference-matches? (cadr ref) v)))]
        [else (assertion-violation 'import "invalid sub-version reference" ref)])]
    [else (assertion-violation 'import "invalid sub-version reference" ref)]))

(define (version-reference-matches? ref version)
  (cond
    [(null? ref) #t]
    [(not (list? ref)) (assertion-violation 'import "invalid version reference" ref)]
    [(memq (car ref) '(and or not))
      (case (car ref)
        [(and) (let loop ([r (cdr ref)])
                 (or (null? r) (and (version-reference-matches? (car r) version) (loop (cdr r)))))]
        [(or) (let loop ([r (cdr ref)])
                (and (pair? r) (or (version-reference-matches? (car r) version) (loop (cdr r)))))]
        [else (and (= (length ref) 2) (not (version-reference-matches? (cadr ref) version)))])]
    [else
      (let loop ([r ref] [v version])
        (cond
          [(null? r) #t]
          [(null? v) #f]
          [else (and (sub-version-reference-matches? (car r) (car v)) (loop (cdr r) (cdr v)))]))]))

;; Signals an error unless the library KEY of version VERSION satisfies the version reference
;; REF of an import.
(define (check-import-version! key version ref)
  (if (not (version-reference-matches? ref version))
    (assertion-violation 'import
      "the version of the library does not satisfy the version reference of the import"
      key version ref)))

;; The version of the library environment ENV, a list.
(define (env-version env) (%env-version env))

;; The version of the library KEY, which is loaded.
(define (library-version key)
  (%env-version (env-for-key (linker-canonical-key key))))

;; Signals an error unless the library KEY, which is loaded, satisfies the version reference
;; REF of an import. `resolve-env-interface` does this when it is given the reference; this is
;; for the expander when it collects errors, which has the interface of the library at hand and
;; reports a version that does not match without giving up the import.
(define (check-library-version! key ref)
  (let ([key (linker-canonical-key key)])
    (check-import-version! key (%env-version (env-for-key key)) ref)))

;;; Import entries

;; Calls THUNK with the view that resolves the import sets of the environment ENV current. The
;; import sets of a library are the names its own package wrote, so they are resolved by the view
;; of that package, and not by the view of whoever imports the library. A library of the
;; toolchain (`core`, `std`) has a plain key and resolves its imports with no view, where a name
;; is its own key: under the importer's view, the `(capy c gen ...)` libraries std generated with
;; `c-include` would be taken for libraries of the importing package, which every package view
;; claims. A namespace (a loaded script, the REPL) and a program have no package of their own:
;; their imports are the names of whoever runs them, and keep the view that is current.
(define (call-with-library-view env thunk)
  (let* ([key (%env-key env)]
         [view (cond
                 [(linker-package-key? key)
                   (or (linker-libmap-view (linker-key-package key)) (current-library-view))]
                 [(and (eq? (%env-kind env) 'library)
                       (not (and (pair? key) (eq? (car key) '%program))))
                   #f]
                 [else (current-library-view)])])
    (if (eq? view (current-library-view))
      (thunk)
      (let ([saved (current-library-view)])
        (dynamic-wind
          (lambda () (current-library-view view))
          thunk
          (lambda () (current-library-view saved)))))))

;; Applies the import entry (mode . import-set) to ENV. 
(define (env-apply-import-entry! env entry)
  (let ([saved ($execution-mode)]
        [mode (car entry)])
    (define (use)
      (call-with-library-view env
        (lambda ()
          (call-with-expansion-env env
            (lambda ()
              (env-use-interfaces! env (list ($resolve-r6rs-interface-hook (cdr entry)))))))))
    (if (eq? mode saved)
      (use)
      (dynamic-wind
        (lambda () ($execution-mode mode))
        use
        (lambda () ($execution-mode saved))))))

(define (env-add-pending-imports! env entries)
  (for-each
    (lambda (entry)
      (if (not (or (member entry (%env-applied env))
                (member entry (%env-pending env))))
        (%env-set-pending! env (append (%env-pending env) (list entry)))))
    entries))

(define (env-force-pending-imports! env)
  (let ([entries (%env-pending env)])
    (%env-set-pending! env '())
    (for-each
      (lambda (entry)
        (env-apply-import-entry! env entry)
        (%env-set-applied! env (cons entry (%env-applied env))))
      entries)))

;;; Lazy library environments

(define (env-register-lazy! key meta inst)
  (core-hash-put! (lazy-envs) key (cons meta inst)))

(define (env-lazy-pending? key)
  (and (core-hash-ref (lazy-envs) key #f) #t))

;; Builds, registers and returns the environment of the lazy library KEY, or #f when
;; there is none.
(define (env-force-lazy! key)
  (let ([entry (core-hash-ref (lazy-envs) key #f)])
    (and entry
      (begin
        (core-hash-remove! (lazy-envs) key)
        (env-fill-from-meta! (register-env! key (make-env key 'library))
          (car entry) (cdr entry))))))

;; Fills the library environment ENV from the META of the library and its instance INST:
;; every variable of the instance is bound in it, the imports are replayed, and the
;; export table is exported through its public interface. Only the export table is
;; public
(define (env-fill-from-meta! env decl inst)
  (let ([public (env-public-interface env)])
    (env-begin-unit! env (library-decl-version decl))
    (%env-set-decl! env decl)
    (%env-set-language! env (library-decl-language decl))
    (for-each
      (lambda (entry)
        (if (eq? (export-entry-kind entry) 'macro)
          (env-note-macro! env (export-entry-internal entry))))
      (library-decl-exports decl))
    (for-each
      (lambda (sym) (env-add! env sym (instance-variable inst sym)))
      (instance-symbols inst))
    (for-each
      (lambda (entry) (env-apply-import-entry! env entry))
      (library-decl-imports decl))
    (env-refresh-exports! env decl)
    env))

;; (Re)binds the export table of META in the public interface of ENV to the current
;; variables of ENV.
(define (env-refresh-exports! env decl)
  (let ([public (env-public-interface env)]
        [replacements (library-decl-replacements decl)])
    (for-each
      (lambda (entry)
        (let ([var (env-variable env (export-entry-internal entry))])
          (if var
            (begin
              (if (memq (export-entry-external entry) replacements)
                (core-hash-put! (%env-replacements public) (export-entry-external entry) #t))
              (env-add! public (export-entry-external entry) var
                (env-origin env (export-entry-internal entry)))))))
      (library-decl-exports decl))))

;; Whether the library KEY has an environment with bindings, or will build one.
(define (env-registered-defined? key)
  (or (env-lazy-pending? key)
    (let ([env (lookup-env-registered key)])
      (and env (env-defined? env)))))

;; Called when the instance INST of a library with META was made, before its body
;; runs. An environment that psyntax built while compiling the library gets the
;; variables of the instance, otherwise the environment is built when it is first needed.
;; Returns the environment the body should run in, or #f.
(define (env-library-instantiated! meta inst exports)
  (let* ([key (library-decl-key meta)]
         [env (lookup-env-registered key)])
    (cond
      [(and env (env-defined? env))
        (env-mirror-exports! env inst exports)
        (env-refresh-exports! env meta)
        (%env-set-decl! env meta)
        env]
      [env
        (env-fill-from-meta! env meta inst)
        env]
      [else
        (env-register-lazy! key meta inst)
        #f])))

;;; Resolution and definition of library environments

;; The environment for KEY, autoloading its library when it has none yet. It
;; is created when the library cannot be found. 
(define (resolve-env key . _)
  (let ([env (lookup-env key)])
    (if (and env (env-defined? env))
      env
      (begin
        (env-autoload! key)
        (ensure-env key)))))

;; The environment that is the core of every library
(define the-core-env
  (let ([env (ensure-env '(capy))]
        [prims (library-instance (current-library-registry) '(|#%capy|))])
    (for-each
      (lambda (sym)
        (env-add! env sym (instance-variable prims sym) (cons (instance-name prims) sym)))
      (instance-symbols prims))
    ; The public interface of `(capy)` is the core itself: it shares the tables.
    (let ([iface (%make-env '(capy) 'interface (%env-locals env) '() (make-core-hash-eq) #f
                   (make-core-hash-eq) 0 #f #f '() '() (%env-origins env) #f
                   '() 'compat '() '() (make-core-hash-eq) '() #f #f)])
      (%env-set-public! env iface))
    env))

;; Make ENV a library environment of its own that sees the core bindings, and
;; return it. PURE? leaves the core out of the imports (`define-pure-module`).
(define (define-env* name . args)
  (let* ([pure? (and (pair? args) (car args))]
         [version (if (and (pair? args) (pair? (cdr args))) (cadr args) '())]
         [env (ensure-env name)])
    (env-begin-unit! env version)
    (env-public-interface env)
    (if pure?
      (env-remove-import! env the-core-env)
      (if (not (or (eq? env the-core-env) (memq the-core-env (%env-imports env))))
        (%env-set-imports! env (append (%env-imports env) (list the-core-env)))))
    (core-hash-clear! (%env-cache env))
    env))

;; Remove IFACE from the imports of ENV.
(define (env-remove-import! env iface)
  (%env-set-imports! env
    (let keep ([imports (%env-imports env)])
      (cond
        [(null? imports) '()]
        [(eq? (car imports) iface) (keep (cdr imports))]
        [else (cons (car imports) (keep (cdr imports)))])))
  (core-hash-clear! (%env-cache env)))


(define (env-mirror-exports! env inst exports)
  (for-each
    (lambda (sym)
      (env-add! env sym
        (or (instance-variable inst sym)
          (instance-ensure-variable! inst sym))
        (cons (instance-name inst) sym)))
    exports))

;; The linker can use the environments from here on.
(linker-install-environments! error
  current-library-registry current-expansion-env expansion-env current-namespace
  ensure-env env-instance env-library-instantiated! env-mirror-exports!
  env-add-pending-imports! library-decl? env-boot-knowns! the-core-env)

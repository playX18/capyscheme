;;; -*- mode: scheme; coding: utf-8; -*-
;;; Expansion environments: the compile-time bindings of a library, an
;;; interface, or a namespace.
;;;
;;; An environment is a tuple (so it can be built before the record infra
;;; in records.scm loads), with these slots:
;;;   0 tag      '%env
;;;   1 key      library name (list of symbols) or #f
;;;   2 kind     'library | 'interface | 'namespace
;;;   3 locals   eq table: symbol -> variable
;;;   4 imports  list of interface environments, searched in order
;;;   5 cache    eq table: symbol -> variable found through imports
;;;   6 public   the public interface environment, or #f until created
;;;   7 replacements  eq table: symbol -> #t
;;;   8 unique-id     counter for gensym and unique ids
;;;   9 meta     free slot for the owner (unused)
;;;  10 exports  for interface environments, the same table as `locals`, #f otherwise
;;;  11 instance for namespaces, the mutable instance whose variables are `locals` (see namespace.scm), #f otherwise
;;;  12 frozen   for namespaces, #t once `namespace-freeze!` has been called
;;;  13 unit     the expansion-time record of the unit being compiled in this
;;;              environment: #(version exports imports) (see `env-begin-unit!`)
;;;  14 pending  import entries not applied yet (see `env-add-pending-imports!`)
;;;  15 applied  import entries already applied to the environment
;;;  16 origins  eq table: symbol -> (key . sym), the binding of the symbol in `locals`
;;;              as its owner names it: the key of the linklet that defines the variable
;;;              and the name it has there. The bindings that come through imports have
;;;              theirs in the imports (see `env-origin`).
;;;  17 registry for namespaces, the library registry their libraries live in (see
;;;              `current-library-registry`), #f otherwise
;;;
;;; Each library registry maps library keys to environments.

(|#%require| (boot prim) (boot control)
  (late (boot raise) error error-without-irritants)
  (late (boot format) format)
  (late (boot psyntax-exp) resolve-r6rs-interface)
  (late (boot library-manager) capy:execution-mode library-defined? load-library
    library-not-found-message))
(|#%provide| all-defined)

;;; Slots

(define (%env-key env) (tuple-ref env 1))
(define (%env-kind env) (tuple-ref env 2))
(define (%env-locals env) (tuple-ref env 3))
;; The imports of ENV. Imports a loaded script left for ENV (see
;; `env-add-pending-imports!`) are applied the first time they are asked for.
(define (%env-imports env)
  (if (pair? (tuple-ref env 14))
    (env-force-pending-imports! env))
  (tuple-ref env 4))
(define (%env-set-imports! env imports) (tuple-set! env 4 imports))
(define (%env-cache env) (tuple-ref env 5))
(define (%env-public env) (tuple-ref env 6))
(define (%env-set-public! env public) (tuple-set! env 6 public))
(define (%env-replacements env) (tuple-ref env 7))
(define (%env-origins env) (tuple-ref env 16))
(define (%env-unique-id env) (tuple-ref env 8))
(define (%env-set-unique-id! env id) (tuple-set! env 8 id))

;; Whether X is an environment
(define (env? x)
  (and (tuple? x)
    (> (tuple-size x) 17)
    (eq? (tuple-ref x 0) '%env)))

(define env-kinds '(library interface namespace))

;; Whether X is a namespace: an environment of kind 'namespace (namespace.scm).
(define (namespace? x)
  (and (env? x) (eq? (%env-kind x) 'namespace)))

;; The instance of namespace NS, or #f.
(define (env-instance env) (tuple-ref env 11))
(define (env-set-instance! env inst) (tuple-set! env 11 inst))

;; Whether namespace NS refuses new definitions (see `namespace-freeze!`).
(define (env-frozen? env) (tuple-ref env 12))
(define (env-set-frozen! env frozen?) (tuple-set! env 12 frozen?))

;; A new environment of KEY and KIND (see the slot list above). Interface
;; environments keep their bindings in `exports`, which is the same table as
;; `locals`, so `env-add!` on an interface fills its exports.
(define (make-env key kind)
  (if (not (memq kind env-kinds))
    (assertion-violation 'make-env "invalid environment kind" kind))
  (let* ([locals (make-core-hash-eq)])
    (tuple '%env key kind locals '() (make-core-hash-eq) #f (make-core-hash-eq) 0 #f
      (if (eq? kind 'interface) locals #f)
      #f #f #f '() '() (make-core-hash-eq) #f)))

(define (env-key env) (%env-key env))
(define (env-kind env) (%env-kind env))
(define (env-meta env) (tuple-ref env 9))
(define (set-env-meta! env meta) (tuple-set! env 9 meta))

;; The exports table of an interface environment, or #f.
(define (env-exports env) (tuple-ref env 10))

;; The environment expansion runs in. Entry points (`compile-tree-il`, `eval`)
;; set it for the extent of the expansion. When it is not set, the core
;; environment is used (boot files expand in it).
(define current-expansion-env (make-parameter #f))

;; The namespace of the REPL, `eval`, and `load` of non-library scripts. See
;; namespace.scm.
(define current-namespace (make-parameter #f))

;; The library registry of namespace NS, or #f when NS has none (an environment that is not
;; a namespace).
(define (env-library-registry env) (tuple-ref env 17))
(define (env-set-library-registry! env reg) (tuple-set! env 17 reg))

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
                (env-note-export! env external internal replace?)
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

;; A new registry named NAME that starts out with the core libraries of PARENT, instances
;; and environments (see `core-library-key?`), and loads the other libraries with RESOLVER,
;; a procedure of a library key, or not at all when RESOLVER is #f (a sealed registry).
(define (make-child-library-registry name parent resolver)
  (let ([reg (make-library-registry name parent resolver)])
    (for-each
      (lambda (key)
        (if (and (core-library-key? key) (not (library-instance reg key)))
          (register-library-instance! reg key (library-instance parent key))))
      (registered-library-keys parent))
    (for-each
      (lambda (entry)
        (if (core-library-key? (car entry))
          (core-hash-put! (registry-envs reg) (car entry) (cdr entry))))
      (core-hash->alist (registry-envs parent)))
    (for-each
      (lambda (entry)
        (if (core-library-key? (car entry))
          (core-hash-put! (registry-lazy-envs reg) (car entry) (cdr entry))))
      (core-hash->alist (registry-lazy-envs parent)))
    reg))

;; The environment registered under KEY, creating a library environment if
;; there is none.
(define (ensure-env key)
  (or (lookup-env key)
    (register-env! key (make-env key 'library))))

;; A hook called with KEY when `resolve-env-interface` finds no environment
;; for it, so the loader can load the library. By default it autoloads the
;; module of that name, and the loader registers the environment as it loads
;; the library. The hook's return value is ignored; the registry is consulted
;; afterwards. A #f hook disables autoloading.
(define env-autoload-hook
  (make-parameter (lambda (key) (if (not (library-defined? key)) (load-library key)))))

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

;; The public interface of the environment for KEY, optionally restricted by
;; SELECT (a list of symbols or (orig . seen) specs), HIDE (names to leave
;; out), and PREFIX (a symbol added to every exported name).
(define (resolve-env-interface key select hide prefix)
  (let* ([env (env-for-key key)]
         [public (%env-public env)]
         [renamer (if prefix
                    (lambda (sym) (symbol-append prefix sym))
                    (lambda (sym) sym))])
    ;; A missing library is reported, and the import binds nothing: the error
    ;; returns to the caller, so the empty interface stands in for the library.
    (cond
      [(not public)
        (error-without-irritants (library-not-found-message key))
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

;; Build a library environment for KEY from a runtime instance. EXPORT-TABLE
;; is an alist ((public-sym . internal-sym) ...). 
(define (instance->interface-env key inst export-table)
  (let* ([lib (make-env key 'library)]
         [public (env-public-interface lib)])
    (for-each
      (lambda (entry)
        (let* ([public-name (car entry)]
               [internal (cdr entry)]
               [var (or (instance-variable inst internal)
                      (instance-ensure-variable! inst internal))])
          (env-add! lib internal var (cons key internal))
          (env-add! public public-name var (cons key internal))))
      export-table)
    (register-env! key lib)))

;;; Library units: what a compiled library tells the loader

;; A library linklet carries its expansion-time facts in its meta, so the environment
;; of the library can be rebuilt from the meta and the instance without running the
;; library body:
;;
;;   (#%library key version exports imports replacements)
;;
;;   key           the library name, a list of symbols and integers
;;   version       the version reference of the library form, a list (maybe empty)
;;   exports       ((public internal kind) ...), kind is `variable` or `macro`. INTERNAL is
;;                 the name the library binds: a definition of the instance, or a binding
;;                 of an import (a re-export)
;;   imports       ((mode . import-set) ...) in the order they were imported. MODE is the
;;                 execution mode the import set was read in, which decides how names
;;                 like `(srfi 1)` and `(srfi :1)` resolve
;;   replacements  the public names exported with `export!`
;;
;; The meta of a namespace linklet is `(#%namespace key . imports)`, where IMPORTS are the
;; import entries the unit ran in the namespace.


(define (env-begin-unit! env version)
  (tuple-set! env 13 (vector version '() '() #t)))

(define (%env-unit env)
  (or (tuple-ref env 13)
    (let ([unit (vector '() '() '() #f)])
      (tuple-set! env 13 unit)
      unit)))

;; Note that ENV exports INTERNAL as PUBLIC.
(define (env-note-export! env public internal replace?)
  (let ([unit (%env-unit env)])
    (vector-set! unit 1 (cons (list public internal replace?) (vector-ref unit 1)))))

;; Record the import set SPE that was imported into ENV, and note that it
;; is applied
(define (env-record-import! env spec)
  (let ([unit (%env-unit env)]
        [entry (cons (capy:execution-mode) spec)])
    (vector-set! unit 2 (cons entry (vector-ref unit 2)))
    (tuple-set! env 15 (cons entry (tuple-ref env 15)))))

;; The import entries recorded in ENV since the last call, oldest first.
(define (env-take-unit-imports! env)
  (let* ([unit (%env-unit env)]
         [imports (let loop ([in (reverse (vector-ref unit 2))] [out '()])
                    (cond
                      [(null? in) (reverse out)]
                      [(member (car in) out) (loop (cdr in) out)]
                      [else (loop (cdr in) (cons (car in) out))]))])
    (vector-set! unit 2 '())
    imports))

;; Whether imports were recorded in ENV that no unit has taken yet.
(define (env-unit-imports? env)
  (pair? (vector-ref (%env-unit env) 2)))

;; The internal names the library of ENV exports, as noted so far.
(define (env-export-internals env)
  (map cadr (vector-ref (%env-unit env) 1)))

;; The meta of the linklet of the library in ENV
(define (library-meta env)
  (let* ([unit (%env-unit env)]
         [seen '()]
         [entries
           ; Newest first, so the first entry of a public name is the one that stays.
           (let loop ([log (vector-ref unit 1)] [out '()])
             (cond
               [(null? log) out]
               [(memq (car (car log)) seen) (loop (cdr log) out)]
               [else
                 (set! seen (cons (car (car log)) seen))
                 (loop (cdr log) (cons (car log) out))]))])
    (list '|#%library|
      (env-key env)
      (vector-ref unit 0)
      (map (lambda (entry)
            (let ([var (env-variable env (cadr entry))])
              (list (car entry) (cadr entry)
                (if (and var (variable-bound? var) (macro? (variable-ref var)))
                  'macro
                  'variable))))
        entries)
      (reverse (vector-ref unit 2))
      (let loop ([es entries] [out '()])
        (cond
          [(null? es) (reverse out)]
          [(caddr (car es)) (loop (cdr es) (cons (car (car es)) out))]
          [else (loop (cdr es) out)])))))

;; The meta of the linklet of a unit that ran in the namespace ENV
(define (namespace-meta env)
  (cons '|#%namespace| (cons (env-key env) (env-take-unit-imports! env))))

(define (library-meta? meta)
  (and (pair? meta) (eq? (car meta) '|#%library|)))

(define (library-meta-key meta) (cadr meta))
(define (library-meta-version meta) (caddr meta))
(define (library-meta-exports meta) (list-ref meta 3))
(define (library-meta-imports meta) (list-ref meta 4))
(define (library-meta-replacements meta) (list-ref meta 5))

;;; Import entries

;; Applies the import entry (mode . import-set) to ENV.
(define (env-apply-import-entry! env entry)
  (let ([saved (capy:execution-mode)]
        [mode (car entry)])
    (define (use)
      (env-use-interfaces! env (list (resolve-r6rs-interface (cdr entry)))))
    (if (eq? mode saved)
      (use)
      (dynamic-wind
        (lambda () (capy:execution-mode mode))
        use
        (lambda () (capy:execution-mode saved))))))

(define (env-add-pending-imports! env entries)
  (for-each
    (lambda (entry)
      (if (not (or (member entry (tuple-ref env 15))
                (member entry (tuple-ref env 14))))
        (tuple-set! env 14 (append (tuple-ref env 14) (list entry)))))
    entries))

(define (env-force-pending-imports! env)
  (let ([entries (tuple-ref env 14)])
    (tuple-set! env 14 '())
    (for-each
      (lambda (entry)
        (env-apply-import-entry! env entry)
        (tuple-set! env 15 (cons entry (tuple-ref env 15))))
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
(define (env-fill-from-meta! env meta inst)
  (let ([public (env-public-interface env)])
    (env-begin-unit! env (library-meta-version meta))
    (for-each
      (lambda (sym) (env-add! env sym (instance-variable inst sym)))
      (instance-symbols inst))
    (for-each
      (lambda (entry) (env-apply-import-entry! env entry))
      (library-meta-imports meta))
    (env-refresh-exports! env meta)
    env))

;; (Re)binds the export table of META in the public interface of ENV to the current
;; variables of ENV.
(define (env-refresh-exports! env meta)
  (let ([public (env-public-interface env)]
        [replacements (library-meta-replacements meta)])
    (for-each
      (lambda (entry)
        (let ([var (env-variable env (cadr entry))])
          (if var
            (begin
              (if (memq (car entry) replacements)
                (core-hash-put! (%env-replacements public) (car entry) #t))
              (env-add! public (car entry) var (env-origin env (cadr entry)))))))
      (library-meta-exports meta))))

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
  (let* ([key (library-meta-key meta)]
         [env (lookup-env-registered key)])
    (cond
      [(and env (env-defined? env))
        (env-mirror-exports! env inst exports)
        (env-refresh-exports! env meta)
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
        (if (not (library-defined? key)) (load-library key))
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
    (let ([iface (make-env '(capy) 'interface)]
          [locals (%env-locals env)])
      (tuple-set! iface 3 locals)
      (tuple-set! iface 10 locals)
      (tuple-set! iface 16 (%env-origins env))
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

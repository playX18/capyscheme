;;; -*- mode: scheme; coding: utf-8; -*-
;;; Namespaces: the top-level environments of the REPL, `eval`, `environment`, and
;;; `load` of non-library scripts.
;;;
;;; A namespace is an environment of kind 'namespace (see environments.scm). Its
;;; variables live in one eq table, which is two things at once: the `locals` of the
;;; environment and the `vars` of its mutable instance (`env-instance`).
;;;
;;; Imports go through psyntax's `import` form, which resolves each interface with
;;; `resolve-env-interface`.

(|#%require| (boot prim) (boot control) (boot raise) (boot format)
  (boot environments) (boot library-manager) (boot eval) (boot psyntax-exp))
(|#%provide| all-defined)

(define namespace-count 0)

;; A fresh key for a namespace made without a name.
(define (fresh-namespace-key)
  (set! namespace-count (+ namespace-count 1))
  (list 'namespace namespace-count))

;; The environment of a namespace whose key is NAME, or a fresh key when NAME is #f.
;; IMPORTS is a list of import specs, imported in order. The namespace is registered
;; under its key, so the hygiene labels of syntax expanded in it resolve back to it.
;; REGISTRY is the library registry the namespace's libraries live in, the one of the
;; current namespace by default.
(define (make-namespace . args)
  (let* ([name (and (pair? args) (car args))]
         [imports (if (and (pair? args) (pair? (cdr args))) (cadr args) '())]
         [registry (or (and (pair? args) (pair? (cdr args)) (pair? (cddr args)) (caddr args))
                     (current-library-registry))]
         [key (or name (fresh-namespace-key))]
         [env (make-env key 'namespace)]
         [locals (%env-locals env)])
    (env-set-instance! env (make-linklet-instance/vars key locals #t))
    (env-set-library-registry! env registry)
    (core-hash-put! (registry-envs registry) key env)
    ; The imports are resolved in the registry of the namespace.
    (call-with-current-namespace env
      (lambda ()
        (for-each (lambda (spec) (namespace-import! env spec)) imports)))
    env))

;; The library registry of namespace NS.
(define (namespace-registry ns)
  (env-library-registry (namespace-env ns)))

;; A namespace in a registry of its own, which sees the libraries of IMPORTS (import specs)
;; and instantiates them afresh: a library has its own state there, apart from the same
;; library in other registries.
(define (make-sandbox-namespace . args)
  (let* ([imports (if (pair? args) (car args) '())]
         [loader (if (and (pair? args) (pair? (cdr args)))
                   (cadr args)
                   load-path-library-resolver)]
         [name (fresh-namespace-key)]
         [registry (make-child-library-registry name (root-library-registry) loader)])
    (make-namespace name imports registry)))

;; Shares the instance of the library KEY from namespace FROM-NS with namespace TO-NS,
;; with the instances of the libraries it imports. Both then see the same state of the
;; library. It is an error when KEY has no instance in FROM-NS, or when TO-NS already
;; has a different instance of KEY or of one of its imports.
(define (namespace-attach-library! from-ns key to-ns)
  (%attach-library! from-ns (namespace-registry from-ns) key (namespace-registry to-ns)))

(define (%attach-library! from-ns from key to)
  (let ([inst (library-instance from key)]
        [existing (library-instance to key)])
    (cond
      [(not inst)
        (assertion-violation 'namespace-attach-library! "library has no instance" key)]
      [existing
        (if (not (eq? existing inst))
          (assertion-violation 'namespace-attach-library!
            "the target has another instance of the library" key))]
      [else
        (register-library-instance! to key inst)
        (let ([env (call-with-current-namespace from-ns (lambda () (lookup-env key)))])
          (if env
            (begin
              (core-hash-put! (registry-envs to) key env)
              (for-each
                (lambda (import)
                  (let ([import-key (env-key import)])
                    (if (and import-key (library-instance from import-key))
                      (%attach-library! from-ns from import-key to))))
                (%env-imports env)))))])))

(define (namespace-env ns)
  (if (namespace? ns)
    ns
    (assertion-violation 'namespace-env "not a namespace" ns)))

;; The mutable instance of namespace NS
(define (namespace-instance ns)
  (env-instance (namespace-env ns)))

;; The key of namespace NS
(define (namespace-name ns)
  (env-key (namespace-env ns)))

;; The value of SYM in NS, through its imports too. A missing binding is an error unless
;; DEFAULT is given.
(define (namespace-ref ns sym . default)
  (apply env-ref (namespace-env ns) sym default))

(define (%namespace-check-defining! env)
  (if (env-frozen? env)
    (assertion-violation 'namespace "namespace is frozen" (env-key env))))

;; Assigns SYM in NS
(define (namespace-set! ns sym val)
  (let* ([env (namespace-env ns)]
         [var (env-local-variable env sym)])
    (cond
      [var (variable-set! var val)]
      [(env-variable env sym)
        (assertion-violation 'namespace-set! "cannot assign imported variable"
          (env-key env) sym)]
      [else
        (assertion-violation 'namespace-set! "unbound variable" (env-key env) sym)])))

;; Defines SYM in NS, or updates it when it is already defined there.
(define (namespace-define! ns sym val)
  (let ([env (namespace-env ns)])
    (%namespace-check-defining! env)
    (env-define! env sym val)))

;; Imports SPEC into NS, like an `import` form evaluated in NS
(define (namespace-import! ns spec)
  (let ([env (namespace-env ns)])
    (env-use-interfaces!
      env
      (list (resolve-r6rs-interface
              (if (and (pair? spec) (eq? (car spec) 'for) (pair? (cdr spec)))
                (cadr spec)
                spec))))))

;; Stops NS from taking new definitions through `namespace-define!`. R6RS environments
;; are frozen once their imports are in.
(define (namespace-freeze! ns)
  (env-set-frozen! (namespace-env ns) #t))

;; The (symbol . variable) pairs visible in NS: its own bindings first, then those of
;; its imports. A name can appear more than once, so callers take the first.
(define (namespace-bindings ns)
  (let ([out '()])
    (for-each
      (lambda (env)
        (env-for-each
          (lambda (sym var) (set! out (cons (cons sym var) out)))
          env))
      (env-and-imports (namespace-env ns)))
    (reverse out)))

;; The namespace used for interaction: the REPL namespace.
(define (interaction-environment)
  (current-namespace))
